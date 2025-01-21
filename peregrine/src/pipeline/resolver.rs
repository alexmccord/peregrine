use std::{
    ffi::OsString,
    io::{self, Error, ErrorKind},
    path::PathBuf,
};

use crate::trie::TrieMap;

#[derive(Debug)]
pub struct Manifest {
    location: PathBuf,
}

impl Manifest {
    pub fn new(location: PathBuf) -> Manifest {
        Manifest { location }
    }
}

pub trait FileResolver {
    fn find_manifest(&self) -> Option<Manifest>;

    fn read(&self, path: &PathBuf) -> io::Result<String>;
}

pub struct FileSystemIO;

impl FileResolver for FileSystemIO {
    fn find_manifest(&self) -> Option<Manifest> {
        todo!()
    }

    fn read(&self, path: &PathBuf) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

#[derive(Debug)]
pub struct VirtualFile(String);

#[derive(Debug)]
pub struct VirtualFileSystem {
    manifest: Option<Manifest>,
    fs: TrieMap<PathBuf, OsString, VirtualFile>,
}

impl VirtualFileSystem {
    pub fn new() -> VirtualFileSystem {
        VirtualFileSystem {
            manifest: None,
            fs: TrieMap::new(),
        }
    }

    pub fn with_manifest(mut self, manifest: Manifest) -> VirtualFileSystem {
        self.manifest = Some(manifest);
        self
    }

    pub fn mk_file(&mut self, path: PathBuf, content: impl Into<String>) -> io::Result<()> {
        if self.fs.contains_prefix_key(&path) {
            return Err(Error::from(ErrorKind::IsADirectory));
        }

        self.fs.insert(path, VirtualFile(content.into()));
        Ok(())
    }
}

impl FileResolver for VirtualFileSystem {
    fn find_manifest(&self) -> Option<Manifest> {
        Some(Manifest::new(PathBuf::from("../eyrie.prg")))
    }

    fn read(&self, path: &PathBuf) -> io::Result<String> {
        match self.fs.get(path) {
            Some(VirtualFile(content)) => Ok(content.clone()),
            None if self.fs.contains_prefix_key(path) => Err(Error::from(ErrorKind::IsADirectory)),
            None => Err(Error::from(ErrorKind::NotFound)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{io::ErrorKind, path::PathBuf};

    use super::{FileResolver, VirtualFileSystem};

    #[test]
    fn try_reading_a_file() {
        let mut vfs = VirtualFileSystem::new();
        vfs.mk_file(PathBuf::from("./foo/bar.txt"), String::from("hello!"))
            .unwrap();

        let res = vfs.read(&PathBuf::from("./foo/bar.txt"));
        assert!(res.is_ok_and(|s| s == "hello!"));
    }

    #[test]
    fn try_reading_a_directory() {
        let mut vfs = VirtualFileSystem::new();
        vfs.mk_file(PathBuf::from("./foo/bar.txt"), String::from("hello!"))
            .unwrap();

        let res = vfs.read(&PathBuf::from("./foo"));
        assert!(res.is_err_and(|e| e.kind() == ErrorKind::IsADirectory));
    }
}
