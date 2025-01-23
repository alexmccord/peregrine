use std::{
    ffi::OsString,
    fs::File,
    io::{self, Cursor, Error, ErrorKind, Read},
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::trie::TrieMap;

#[derive(Error, Debug)]
pub enum FileResolverError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Build manifest `eyrie.toml` was nowhere to be found in the ancestral path.")]
    ManifestNotFound,
}

impl FileResolverError {
    pub fn as_io(&self) -> Option<&io::Error> {
        match self {
            FileResolverError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl PartialEq for FileResolverError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(_), _) => false,
            (Self::ManifestNotFound, Self::ManifestNotFound) => true,
            (Self::ManifestNotFound, _) => false,
        }
    }
}

pub type Result<T> = std::result::Result<T, FileResolverError>;

pub trait FileResolver {
    type File: Read;

    fn open(&self, path: &Path) -> Result<Self::File>;

    fn read(&self, path: &Path) -> Result<String> {
        self.read_file(self.open(path)?)
    }

    fn read_file(&self, file: Self::File) -> Result<String> {
        Ok(io::read_to_string(file)?)
    }

    fn locate_and_open_manifest_file(&self, search: &Path) -> Result<(PathBuf, Self::File)> {
        for dir in search.ancestors() {
            match self.open(&dir.join("eyrie.toml")) {
                Ok(file) => return Ok((dir.to_owned(), file)),
                Err(FileResolverError::Io(e)) if e.kind() == io::ErrorKind::NotFound => continue,
                Err(e) => return Err(e),
            }
        }

        Err(FileResolverError::ManifestNotFound)
    }
}

pub struct FileSystemIO;

impl FileResolver for FileSystemIO {
    type File = File;

    fn open(&self, path: &Path) -> Result<Self::File> {
        Ok(File::open(path)?)
    }
}

#[derive(Debug, Clone)]
pub struct VirtualFile(Cursor<String>);

impl Read for VirtualFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

#[derive(Debug)]
pub struct VirtualFileSystem {
    fs: TrieMap<PathBuf, OsString, String>,
}

impl VirtualFileSystem {
    pub fn new() -> VirtualFileSystem {
        VirtualFileSystem { fs: TrieMap::new() }
    }

    pub fn write(&mut self, path: PathBuf, content: impl Into<String>) -> io::Result<()> {
        if self.fs.contains_prefix_key(&path) {
            return Err(Error::from(ErrorKind::IsADirectory));
        }

        self.fs.insert(path, content.into());

        Ok(())
    }
}

impl FileResolver for VirtualFileSystem {
    type File = VirtualFile;

    fn open(&self, path: &Path) -> Result<Self::File> {
        match self.fs.get(path) {
            Some(vf) => Ok(VirtualFile(Cursor::new(vf.clone()))),
            None if self.fs.contains_prefix_key(path) => {
                Err(FileResolverError::Io(Error::from(ErrorKind::IsADirectory)))
            }
            None => Err(FileResolverError::Io(Error::from(ErrorKind::NotFound))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{io::ErrorKind, path::PathBuf};

    use crate::pipeline::resolver::FileResolverError;

    use super::{FileResolver, VirtualFileSystem};

    #[test]
    fn try_reading_a_file() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/foo/bar.txt"), "hello!").unwrap();

        let res = vfs.read(&PathBuf::from("/foo/bar.txt")).unwrap();
        assert!(res == "hello!");
    }

    #[test]
    fn try_reading_a_directory() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/foo/bar.txt"), "hello!").unwrap();

        let res = vfs.read(&PathBuf::from("/foo")).unwrap_err();
        assert!(res
            .as_io()
            .is_some_and(|e| e.kind() == ErrorKind::IsADirectory));
    }

    #[test]
    fn find_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/project/eyrie.toml"), "manifest file!")
            .unwrap();
        vfs.write(PathBuf::from("/project/src/foo/bar/baz.prg"), "")
            .unwrap();

        let (location, file) = vfs
            .locate_and_open_manifest_file(&PathBuf::from("/project/src/foo/bar"))
            .unwrap();

        assert_eq!(location, PathBuf::from("/project"));
        assert_eq!(vfs.read_file(file).unwrap(), "manifest file!");
    }

    #[test]
    fn cannot_find_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/project/src/foo/bar/baz.prg"), "")
            .unwrap();

        let err = vfs
            .locate_and_open_manifest_file(&PathBuf::from("/project/src/foo/bar"))
            .unwrap_err();

        assert_eq!(err, FileResolverError::ManifestNotFound);
    }
}
