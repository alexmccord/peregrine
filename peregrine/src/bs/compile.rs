use std::io;
use std::path::PathBuf;

use thiserror::Error;

use crate::bs::module::ModuleCtxt;
use crate::fs::{FileResolver, FileResolverError};
use crate::syntax;

// Formalizing the build system so I can actually think it through systematically.
//
// 1. Command line interface.
//    a. The program is executed with some cwd set somewhere.
//    b. If a path was supplied in the command line incantation, it overrides the cwd.
//    c. Otherwise, the cwd path is used.
//
// 2. BuildDriver.
//    a. The program then submits a task to build the project at the provided path.
//    b. The program can also submit other tasks that the build driver needs to do as well.
//    c. Once populated with the tasks necessary, the program calls `execute()` to resolve all
//       obligations of the submitted tasks in whichever order it deems necessary.
//
// 3. Resolution of a BuildProject task.
//    a. Locate the build manifest along the ancestry.
//    b. If none is found, perhaps we're compiling a single file:
//       i.  If we are, use a default manifest.
//       ii. Otherwise, abort compilation and report missing manifest.
//    c. If one is found, parse and check for validity.
//    d. Then move the build driver to the next phase of building modules.

#[derive(Debug)]
pub struct CompileOptions {
    /// A path to the input .prg file to compile.
    pub input: PathBuf,
    /// A path to the output object file to write to.
    pub output: PathBuf,
}

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum BuildError {
    #[error("Unrecognizable file \"{0}\"")]
    InputFileNotEndingInPrg(PathBuf),
    #[error("{0}")]
    CannotBuildModuleAtDirectory(PathBuf),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Build(#[from] BuildError),
    #[error(transparent)]
    FileResolver(#[from] FileResolverError),
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
