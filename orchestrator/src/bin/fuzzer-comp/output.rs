use std::path::{Path, PathBuf};

use futures::{Stream, StreamExt};
use tokio::io::AsyncWriteExt;

use common::answers::{AsyncAnswersWriter, BinaryFileAnswerError, BinaryFileMultiAnswersWriter};

pub(crate) trait HasBaseBytes {
    fn base_bytes(&self) -> impl AsRef<[u8]> + Send + 'static;
}

pub(crate) trait HasByteAnswers {
    fn answers<'a>(&'a self) -> impl ExactSizeIterator<Item = (usize, Option<u8>)> + Send + 'a;
}

async fn write_output_to_file(path: PathBuf, buffer: Box<[u8]>) -> Result<PathBuf, std::io::Error> {
    let mut file = tokio::fs::File::create(&path).await?;
    file.write_all(&buffer).await?;
    Ok(path.to_owned())
}

pub(crate) fn dump_outputs<O>(
    output_dir: &Path,
    outputs: impl Stream<Item = O> + Send + 'static,
) -> tokio::task::JoinHandle<()>
where
    O: HasBaseBytes + HasByteAnswers + Send,
{
    let mut writer = BinaryFileMultiAnswersWriter::new_with_write_async(
        output_dir.to_path_buf(),
        Some("output_".to_owned()),
        "bin".to_owned(),
        write_output_to_file,
    );

    tokio::task::spawn(async move {
        tokio::pin!(outputs);
        while let Some(output) = outputs.next().await {
            writer.set_default_answers(output.base_bytes());
            let result = writer
                .write(output.answers())
                .await
                .map(|write_result| write_result.map_err(|e| BinaryFileAnswerError::Io(e)))
                .flatten();
            result.expect("Failed to write output");
        }
    })
}
