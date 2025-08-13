use core::{error::Error, fmt::Display, future::Future};
use std::prelude::rust_2021::*;

use super::{log_debug, log_info, log_warn};

/// Outputs answers found for the symbolic values.
pub trait AnswersWriter {
    type Id;
    type Answer;
    type Output;
    type Error;

    fn write(
        &mut self,
        answers: impl ExactSizeIterator<Item = (Self::Id, Self::Answer)>,
    ) -> Result<Self::Output, Self::Error>;
}

/// Outputs answers found for the symbolic values.
pub trait AsyncAnswersWriter {
    type Id;
    type Answer;
    type Output;
    type Error;

    fn write(
        &mut self,
        answers: impl ExactSizeIterator<Item = (Self::Id, Self::Answer)>,
    ) -> impl Future<Output = Result<Self::Output, Self::Error>>;
}

/// Provides a switch for an existing [AnswersWriter].
pub struct SwitchableAnswersWriter<W: AnswersWriter> {
    enabled: bool,
    inner: W,
}

impl<W: AnswersWriter> SwitchableAnswersWriter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            enabled: true,
            inner,
        }
    }

    pub fn switch(&mut self, enabled: bool) {
        self.enabled = enabled;
    }
}

impl<W: AnswersWriter> AnswersWriter for SwitchableAnswersWriter<W> {
    type Id = W::Id;
    type Answer = W::Answer;
    type Output = Result<W::Output, W::Error>;
    type Error = ();

    fn write(
        &mut self,
        answers: impl ExactSizeIterator<Item = (Self::Id, Self::Answer)>,
    ) -> Result<Self::Output, Self::Error> {
        if !self.enabled {
            return Err(());
        }

        Ok(self.inner.write(answers))
    }
}

mod binary {
    use core::ops::Deref;
    use std::{
        format,
        io::{self, Write},
        path::PathBuf,
    };

    use super::*;

    ///
    #[derive(Debug)]
    pub enum BinaryFileAnswerError {
        /// The answer iterator did not provide all the values for the buffer.
        Incomplete,
        /// Answer at an index was not byte-typed.
        NonByte(usize),
        Io(std::io::Error),
    }

    impl Error for BinaryFileAnswerError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            match self {
                Self::Io(err) => err.source(),
                _ => None,
            }
        }
    }

    impl Display for BinaryFileAnswerError {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                BinaryFileAnswerError::Incomplete => write!(f, "Incomplete set of answers"),
                BinaryFileAnswerError::NonByte(index) => {
                    write!(f, "Answer with index {} is not available as a byte", index)
                }
                BinaryFileAnswerError::Io(err) => write!(f, "{err}"),
            }
        }
    }

    pub trait BinaryFileAnswersWriteFn {
        type Output;

        fn call(&mut self, path: PathBuf, buffer: &[u8]) -> Self::Output;
    }

    pub struct DefaultBinaryFileAnswersWriteFn;

    impl BinaryFileAnswersWriteFn for DefaultBinaryFileAnswersWriteFn {
        type Output = Result<PathBuf, io::Error>;

        fn call(&mut self, path: PathBuf, buffer: &[u8]) -> Self::Output {
            log_debug!("Writing values to file: {}", path.display());

            std::fs::File::create(&path)
                .and_then(|mut f| f.write_all(buffer))
                .map(|_| path)
        }
    }

    pub trait BinaryFileAnswersWriteAsyncFn {
        type Output;

        fn call(&mut self, path: PathBuf, buffer: &[u8]) -> impl Future<Output = Self::Output>;
    }

    pub struct AsyncFnMutBinaryFileAnswersWriteFn<F: FnMut(PathBuf, Box<[u8]>) -> Fut, Fut>(pub F);

    impl<F, Fut> BinaryFileAnswersWriteAsyncFn for AsyncFnMutBinaryFileAnswersWriteFn<F, Fut>
    where
        F: FnMut(PathBuf, Box<[u8]>) -> Fut,
        Fut: Future,
    {
        type Output = Fut::Output;

        async fn call(&mut self, path: PathBuf, buffer: &[u8]) -> Self::Output {
            (self.0)(path, buffer.into()).await
        }
    }

    /// Outputs answers found for byte-typed symbolic values over time.
    /// It takes a directory and outputs each answer in a separate file named
    /// with the format `{prefix}{counter}.{extension}`.
    /// # Remarks
    /// - Ids are the index of the byte in the output buffer.
    /// - A `None` answer means it is not byte-typed.
    pub struct BinaryFileMultiAnswersWriter<W = DefaultBinaryFileAnswersWriteFn> {
        dir_path: PathBuf,
        counter: usize,
        prefix: String,
        extension: String,
        buffer: Vec<u8>,
        /// The output buffer will be initially filled with this buffer.
        /// Useful when the output will be used as input again.
        default_answers: Box<dyn AsRef<[u8]> + Send + 'static>,
        write_fn: W,
    }

    impl<W> BinaryFileMultiAnswersWriter<W> {
        pub fn new_with_write(
            dir_path: PathBuf,
            prefix: Option<String>,
            extension: String,
            write_fn: W,
        ) -> Self {
            std::fs::create_dir_all(&dir_path).unwrap();

            log_info!(
                "Setting up binary output writing to directory: {}",
                dir_path.display()
            );
            let dir_path = std::fs::canonicalize(dir_path).unwrap();

            check_out_dir(&dir_path, prefix.as_ref(), &extension);

            Self {
                dir_path,
                counter: 0,
                prefix: prefix.unwrap_or_default(),
                extension,
                buffer: Vec::new(),
                default_answers: Box::new(Vec::new()),
                write_fn,
            }
        }

        fn write_to_buffer(
            &mut self,
            answers: impl ExactSizeIterator<Item = (usize, Option<u8>)>,
        ) -> Result<usize, BinaryFileAnswerError> {
            // The buffer is growing and at least as wide as the default answers.
            let default_answers: &[u8] = self.default_answers.deref().as_ref();
            self.buffer[0..default_answers.len()].copy_from_slice(default_answers.as_ref());
            let mut filled = default_answers.len();
            let mut max_upper = default_answers.len();

            if answers.len() > self.buffer.len() {
                self.buffer.reserve(answers.len() - self.buffer.len());
            }

            for (id, ans) in answers {
                let index = id;
                let byte_ans = ans.ok_or_else(|| BinaryFileAnswerError::NonByte(index))?;

                if index >= max_upper {
                    max_upper = index + 1;
                    if max_upper > self.buffer.len() {
                        self.buffer.resize(max_upper, 0);
                    }
                }
                self.buffer[index] = byte_ans;
                if index >= default_answers.len() {
                    filled += 1;
                }
            }
            // If there are bytes not filled with the default answers nor answers.
            if filled < max_upper {
                return Err(BinaryFileAnswerError::Incomplete);
            }

            Ok(max_upper)
        }

        fn get_path_and_incr(&mut self) -> PathBuf {
            let path = self
                .dir_path
                .join(format!("{}{}", self.prefix, self.counter))
                .with_added_extension(&self.extension);

            self.counter += 1;
            path
        }

        pub fn set_default_answers(&mut self, default_answers: impl AsRef<[u8]> + Send + 'static) {
            self.buffer.resize(default_answers.as_ref().len(), 0);
            self.default_answers = Box::new(default_answers);
        }
    }

    impl BinaryFileMultiAnswersWriter {
        pub fn new_with_write_async<F, Fut>(
            dir_path: PathBuf,
            prefix: Option<String>,
            extension: String,
            write_fn: F,
        ) -> BinaryFileMultiAnswersWriter<AsyncFnMutBinaryFileAnswersWriteFn<F, Fut>>
        where
            F: FnMut(PathBuf, Box<[u8]>) -> Fut,
            Fut: Future,
            AsyncFnMutBinaryFileAnswersWriteFn<F, Fut>: BinaryFileAnswersWriteAsyncFn,
        {
            BinaryFileMultiAnswersWriter::new_with_write(
                dir_path,
                prefix,
                extension,
                AsyncFnMutBinaryFileAnswersWriteFn(write_fn),
            )
        }
    }

    impl BinaryFileMultiAnswersWriter {
        pub fn new(
            dir_path: PathBuf,
            prefix: Option<String>,
            extension: String,
            default_answers: Option<&[u8]>,
        ) -> BinaryFileMultiAnswersWriter {
            let mut instance = BinaryFileMultiAnswersWriter::new_with_write(
                dir_path,
                prefix,
                extension,
                DefaultBinaryFileAnswersWriteFn,
            );

            instance.set_default_answers(
                default_answers
                    .map(|buf| buf.to_owned())
                    .unwrap_or_default(),
            );
            instance
        }
    }

    impl<W> AnswersWriter for BinaryFileMultiAnswersWriter<W>
    where
        W: BinaryFileAnswersWriteFn,
    {
        type Id = usize;
        type Answer = Option<u8>;
        type Output = <W as BinaryFileAnswersWriteFn>::Output;
        type Error = BinaryFileAnswerError;

        fn write(
            &mut self,
            answers: impl ExactSizeIterator<Item = (Self::Id, Self::Answer)>,
        ) -> Result<Self::Output, BinaryFileAnswerError> {
            let max_upper = self.write_to_buffer(answers)?;

            let path = self.get_path_and_incr();
            let result = self.write_fn.call(path, &self.buffer[0..max_upper]);

            Ok(result)
        }
    }

    impl<W> AsyncAnswersWriter for BinaryFileMultiAnswersWriter<W>
    where
        W: BinaryFileAnswersWriteAsyncFn,
    {
        type Id = usize;
        type Answer = Option<u8>;
        type Output = <W as BinaryFileAnswersWriteAsyncFn>::Output;
        type Error = BinaryFileAnswerError;

        async fn write(
            &mut self,
            answers: impl ExactSizeIterator<Item = (Self::Id, Self::Answer)>,
        ) -> Result<Self::Output, BinaryFileAnswerError> {
            let max_upper = self.write_to_buffer(answers)?;

            let path = self.get_path_and_incr();
            let result = self.write_fn.call(path, &self.buffer[0..max_upper]).await;

            Ok(result)
        }
    }

    fn check_out_dir(dir_path: &PathBuf, file_prefix: Option<&String>, file_ext: &str) {
        if std::fs::read_dir(&dir_path)
            .unwrap()
            .filter_map(io::Result::ok)
            .filter(|e| e.metadata().is_ok_and(|t| t.is_file()))
            .any(|e| {
                e.file_name().to_string_lossy().ends_with(file_ext)
                    && file_prefix
                        .is_none_or(|prefix| e.file_name().to_string_lossy().starts_with(prefix))
            })
        {
            log_warn!(
                "Output directory has some previous answers, which may may be overwritten: {}",
                dir_path.display(),
            );
        }
    }
}
pub use binary::{
    BinaryFileAnswerError, BinaryFileAnswersWriteAsyncFn, BinaryFileAnswersWriteFn,
    BinaryFileMultiAnswersWriter,
};
