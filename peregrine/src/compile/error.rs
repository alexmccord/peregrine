use std::{io, path::PathBuf};

use thiserror::Error;

use crate::fs::FileResolverError;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Build(#[from] BuildError),
    #[error(transparent)]
    FileResolver(#[from] FileResolverError),
}

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum BuildError {
    #[error("Unrecognizable file \"{0}\"")]
    InputFileNotEndingInPrg(PathBuf),
    #[error("{0}")]
    CannotBuildModuleAtDirectory(PathBuf),
}

impl Error {
    pub fn as_build(&self) -> Option<&BuildError> {
        match self {
            Error::Build(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_file_resolver(&self) -> Option<&FileResolverError> {
        match self {
            Error::FileResolver(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_io(&self) -> Option<&io::Error> {
        match self {
            Error::Build(_) => None,
            Error::FileResolver(e) => e.as_io(),
        }
    }
}

impl PartialEq<BuildError> for Error {
    fn eq(&self, other: &BuildError) -> bool {
        match (self, other) {
            (Error::Build(e1), e2) => e1 == e2,
            _ => false,
        }
    }
}

impl PartialEq<Error> for BuildError {
    fn eq(&self, other: &Error) -> bool {
        other == self
    }
}

impl PartialEq<FileResolverError> for Error {
    fn eq(&self, other: &FileResolverError) -> bool {
        match (self, other) {
            (Error::FileResolver(e1), e2) => e1 == e2,
            _ => false,
        }
    }
}

impl PartialEq<Error> for FileResolverError {
    fn eq(&self, other: &Error) -> bool {
        other == self
    }
}
