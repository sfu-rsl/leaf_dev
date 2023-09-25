use std::{
    fs::{File, OpenOptions},
    io::{Read, Seek, Write},
    ops::{Deref, DerefMut},
};

#[derive(Default)]
pub(crate) struct Chain<A, B> {
    pub first: A,
    pub second: B,
}

/* NOTE: The trailing comma is mandatory for this macro.
 * Also note that a kind of expression is path, so it is important to
 * distinguish them in the pattern.
 */
macro_rules! chain {
    (<$head:path>, $($tail:tt)*) => {
        crate::utils::chain!((<$head>::default()), $($tail)*)
    };
    ($single:expr,) => {
        $single
    };
    ($head:expr, $($tail:tt)+) => {
        crate::utils::Chain {
            first: $head,
            second: crate::utils::chain!($($tail)+)
        }
    };
}
pub(crate) use chain;

pub(crate) struct TypeExport {
    file: File,
}

impl TypeExport {
    pub fn new() -> Self {
        let file = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .truncate(true)
            .open("types.json")
            .expect("Unable to open file for type export");

        TypeExport { file }
    }

    pub fn read(&mut self, content: &mut String) {
        self.file.read_to_string(content).unwrap();
    }

    pub fn overwrite(&mut self, content: String) {
        self.clean();
        self.write(content);
    }

    fn clean(&mut self) {
        self.file.set_len(0).unwrap();
        self.file.seek(std::io::SeekFrom::Start(0)).unwrap();
    }

    fn write(&mut self, content: String) {
        self.file.write_all(content.as_bytes()).unwrap();
        self.file.seek(std::io::SeekFrom::Start(0)).unwrap();
    }
}

impl Deref for TypeExport {
    type Target = File;

    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

impl DerefMut for TypeExport {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.file
    }
}
