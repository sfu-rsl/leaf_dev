use std::path::{Path, PathBuf};

use futures::{Stream, StreamExt, TryStream, TryStreamExt};
use tokio::{pin, task::JoinHandle};

use common::{log_debug, log_info};

use crate::Input;

type GenericError = Box<dyn core::error::Error + Send + Sync>;

type Result<T> = std::result::Result<T, GenericError>;

mod corpus {
    use notify::Watcher;
    use tokio::sync::mpsc;
    use tokio_stream::wrappers::{ReadDirStream, UnboundedReceiverStream};

    use super::*;

    pub(super) async fn read_all_files(
        dir: &Path,
    ) -> Result<impl TryStream<Ok = PathBuf, Error = GenericError> + use<>> {
        let read_dir = ReadDirStream::new(tokio::fs::read_dir(dir).await?);
        Ok(read_dir.filter_map(async |entry| match entry {
            Ok(entry) => {
                let file_type = match entry.file_type().await {
                    Ok(file_type) => file_type,
                    Err(e) => {
                        return Some(Err(e.into()));
                    }
                };
                if file_type.is_file() {
                    Some(Ok(entry.path()))
                } else {
                    None
                }
            }
            Err(e) => Some(Err(e.into())),
        }))
    }

    struct TokioSenderAdapter(mpsc::UnboundedSender<notify::Result<notify::Event>>);

    impl notify::EventHandler for TokioSenderAdapter {
        fn handle_event(&mut self, event: notify::Result<notify::Event>) {
            let _ = self.0.send(event);
        }
    }

    pub(super) fn watch_dir(
        dir: &Path,
    ) -> Result<(
        impl Drop + use<>,
        impl TryStream<Ok = PathBuf, Error = GenericError> + use<>,
    )> {
        let (tx, rx) = mpsc::unbounded_channel();
        let mut watcher = notify::recommended_watcher(TokioSenderAdapter(tx))?;
        watcher.watch(dir, notify::RecursiveMode::NonRecursive)?;
        Ok((
            watcher,
            UnboundedReceiverStream::new(rx).filter_map(async |event| match event {
                Ok(event) => {
                    if event.kind.is_create() && event.paths.len() == 1 {
                        Some(Ok(event.paths[0].clone()))
                    } else {
                        None
                    }
                }
                Err(e) => Some(Err(e.into())),
            }),
        ))
    }
}

mod process {
    use std::path::PathBuf;

    use crate::utils::{no_order_wrapper, priority_channel};

    use super::*;

    no_order_wrapper!(NoOrderInput(Input));

    pub(super) struct NoOpCorpusInputProcessor {
        inputs_tx: priority_channel::Sender<NoOrderInput>,
    }

    impl NoOpCorpusInputProcessor {
        pub fn new() -> (Self, impl Stream<Item = Input>) {
            let (tx, rx) = priority_channel::channel::<NoOrderInput>();
            (Self { inputs_tx: tx }, rx.to_stream().map(|input| input.0))
        }

        pub(super) async fn process_existing(&self, inputs: impl Stream<Item = Result<PathBuf>>) {
            self.add_all(inputs).await
        }

        pub(super) async fn process_new(&self, inputs: impl Stream<Item = Result<PathBuf>>) {
            self.add_all(inputs).await
        }

        async fn add_all(&self, inputs: impl Stream<Item = Result<PathBuf>>) {
            pin!(inputs);
            while let Some(input) = inputs.next().await {
                let Ok(path) = input else {
                    log_debug!("Error reading path: {:?}", input);
                    continue;
                };

                self.inputs_tx.send(self.to_input(path).into()).await;
            }
        }

        fn to_input(&self, path: PathBuf) -> Input {
            Input { path }
        }
    }
}

pub(crate) async fn prioritized_inputs(
    inputs_dir: PathBuf,
    offline: bool,
) -> Result<(JoinHandle<()>, impl Stream<Item = Input>)> {
    let (processor, inputs) = process::NoOpCorpusInputProcessor::new();

    let existing = corpus::read_all_files(inputs_dir.as_ref()).await?;
    let new_files = if !offline {
        Some(corpus::watch_dir(inputs_dir.as_ref())?)
    } else {
        None
    };

    let process_handle = tokio::task::spawn(async move {
        let mut counter = 0;
        processor
            .process_existing(existing.into_stream().inspect(|_| counter += 1))
            .await;

        log_info!("Processed {} existing inputs", counter);

        if let Some((watch_handle, new_live)) = new_files {
            log_info!("Watching for new inputs ...");

            processor.process_new(new_live.into_stream()).await;
            drop(watch_handle);
        }
    });

    Ok((process_handle, inputs))
}
