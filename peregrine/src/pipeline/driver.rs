use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use thiserror::Error;

use crate::pipeline::module::ModuleCtxt;
use crate::pipeline::resolver::FileResolver;
use crate::pipeline::tasks::TaskGraph;
use crate::syntax::parser::Parser;

use super::manifest::{Manifest, ManifestError};
use super::resolver::FileResolverError;

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
pub enum BuildTask {
    BuildProject(PathBuf),
    BuildModule(PathBuf),
}

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum BuildError {
    #[error("")]
    PathMustBeAPrgFile(PathBuf),
    #[error("")]
    CannotBuildModuleAtDirectory,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Build(#[from] BuildError),
    #[error(transparent)]
    FileResolver(#[from] FileResolverError),
    #[error(transparent)]
    Manifest(#[from] ManifestError),
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

    pub fn as_manifest(&self) -> Option<&ManifestError> {
        match self {
            Error::Manifest(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_io(&self) -> Option<&io::Error> {
        match self {
            Error::Build(_) => None,
            Error::FileResolver(e) => e.as_io(),
            Error::Manifest(_) => None,
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

impl PartialEq<ManifestError> for Error {
    fn eq(&self, other: &ManifestError) -> bool {
        match (self, other) {
            (Error::Manifest(e1), e2) => e1 == e2,
            _ => false,
        }
    }
}

impl PartialEq<Error> for ManifestError {
    fn eq(&self, other: &Error) -> bool {
        other == self
    }
}

pub struct BuildDriver<R> {
    tasks: TaskGraph<BuildTask>,
    resolver: R,
    manifest: Option<(PathBuf, Manifest)>,
    modules: HashMap<PathBuf, ModuleCtxt>,
}

impl<R: FileResolver> BuildDriver<R> {
    pub fn new(resolver: R) -> BuildDriver<R> {
        BuildDriver {
            tasks: TaskGraph::new(),
            resolver,
            manifest: None,
            modules: HashMap::new(),
        }
    }

    pub fn submit_task(&mut self, task: BuildTask) {
        self.tasks.submit(task);
    }

    pub fn execute(&mut self) -> Result<()> {
        while let Some(submitted) = self.tasks.pop() {
            match submitted.task {
                BuildTask::BuildProject(path) => self.build_project(path)?,
                BuildTask::BuildModule(path) => self.build_module(path)?,
            }
        }

        Ok(())
    }

    fn build_project(&mut self, path: PathBuf) -> Result<()> {
        if self.manifest.is_none() {
            let (location, file) = self.resolver.locate_and_open_manifest_file(&path)?;
            let content = self.resolver.read_file(file)?;

            self.manifest = Some((location, Manifest::try_from(content)?));
        }

        self.submit_task(BuildTask::BuildModule(path));

        Ok(())
    }

    fn build_module(&mut self, path: PathBuf) -> Result<()> {
        if path.extension().map_or(true, |ext| ext != "prg") {
            return Err(Error::Build(BuildError::PathMustBeAPrgFile(path)));
        }

        let result = Parser::parse(self.resolver.read(&path)?);
        self.modules.insert(path, ModuleCtxt::new(result));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::pipeline::driver::{BuildError, BuildTask};
    use crate::pipeline::resolver::{FileResolverError, VirtualFileSystem};

    use super::BuildDriver;

    #[test]
    fn build_project() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/eyrie.toml"), "").unwrap();
        vfs.write(PathBuf::from("/foo.prg"), "").unwrap();

        let mut driver = BuildDriver::new(vfs);
        driver.submit_task(BuildTask::BuildProject(PathBuf::from("/foo.prg")));
        let res = driver.execute();

        assert!(res.is_ok());
    }

    #[test]
    fn build_project_at_dir_errors() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/eyrie.toml"), "").unwrap();
        vfs.write(PathBuf::from("/foo/bar.prg"), "").unwrap();

        let mut driver = BuildDriver::new(vfs);
        driver.submit_task(BuildTask::BuildProject(PathBuf::from("/foo")));
        let res = driver.execute();

        assert!(res.is_err_and(|e| e == BuildError::PathMustBeAPrgFile(PathBuf::from("/foo"))));
    }

    #[test]
    fn build_project_without_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/foo.prg"), "").unwrap();

        let mut driver = BuildDriver::new(vfs);
        driver.submit_task(BuildTask::BuildProject(PathBuf::from("/foo.prg")));
        let res = driver.execute();

        assert!(res.is_err_and(|e| e == FileResolverError::ManifestNotFound));
    }
}
