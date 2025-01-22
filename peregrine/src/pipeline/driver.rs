use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use thiserror::Error;

use crate::pipeline::module::ModuleCtxt;
use crate::pipeline::resolver::FileResolver;
use crate::pipeline::tasks::{Task, TaskGraph};
use crate::syntax::parser::Parser;

use super::tasks::Resolution;

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

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum BuildError {
    #[error("")]
    ManifestNotFound,
    #[error("")]
    PathMustBeAPrgFile(PathBuf),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Build(#[from] BuildError),
    #[error(transparent)]
    Io(#[from] io::Error),
}

impl Error {
    pub fn as_build(&self) -> Option<&BuildError> {
        match self {
            Error::Build(e) => Some(e),
            Error::Io(_) => None,
        }
    }

    pub fn as_io(&self) -> Option<&io::Error> {
        match self {
            Error::Build(_) => None,
            Error::Io(e) => Some(e),
        }
    }
}

impl PartialEq<BuildError> for Error {
    fn eq(&self, other: &BuildError) -> bool {
        match (self, other) {
            (Error::Build(e1), e2) => e1 == e2,
            (Error::Io(_), _) => false,
        }
    }
}

impl PartialEq<Error> for BuildError {
    fn eq(&self, other: &Error) -> bool {
        other == self
    }
}

pub struct BuildDriver<R> {
    tasks: TaskGraph,
    resolver: R,
    modules: HashMap<PathBuf, ModuleCtxt>,
}

impl<R: FileResolver> BuildDriver<R> {
    pub fn new(resolver: R) -> BuildDriver<R> {
        BuildDriver {
            tasks: TaskGraph::new(),
            resolver,
            modules: HashMap::default(),
        }
    }

    pub fn submit_task(&mut self, task: Task) {
        self.tasks.submit(task);
    }

    pub fn execute(&mut self) -> Result<(), Error> {
        while let Some(submitted) = self.tasks.pop() {
            match submitted.task {
                Task::BuildProject(path) => self.build_project(path)?,
                Task::BuildModule(path) => self.build_module(path)?,
            }
        }

        Ok(())
    }

    fn build_project(&mut self, path: PathBuf) -> Result<(), Error> {
        self.submit_task(Task::BuildModule(path));
        Ok(())
    }

    fn build_module(&mut self, path: PathBuf) -> Result<(), Error> {
        let ext = path
            .extension()
            .ok_or(Error::Io(io::Error::from(io::ErrorKind::IsADirectory)))?;

        if ext != "prg" {
            return Err(Error::Build(BuildError::PathMustBeAPrgFile(path)));
        }

        let result = Parser::parse(self.resolver.read(&path)?);
        self.modules.insert(path, ModuleCtxt::new(result));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::ErrorKind;
    use std::path::PathBuf;

    use crate::pipeline::resolver::VirtualFileSystem;
    use crate::pipeline::tasks::Task;

    use super::BuildDriver;

    #[test]
    fn build_project() {
        let mut vfs = VirtualFileSystem::new();
        vfs.mk_file(PathBuf::from("./foo.prg"), "").unwrap();

        let mut driver = BuildDriver::new(vfs);
        driver.submit_task(Task::BuildProject(PathBuf::from("./foo.prg")));
        let res = driver.execute();

        assert!(res.is_ok());
    }

    #[test]
    fn build_project_at_dir_errors() {
        let mut vfs = VirtualFileSystem::new();
        vfs.mk_file(PathBuf::from("./foo/bar.prg"), "").unwrap();

        let mut driver = BuildDriver::new(vfs);
        driver.submit_task(Task::BuildProject(PathBuf::from("./foo")));
        let res = driver.execute();

        assert!(res.is_err_and(|e| e.as_io().unwrap().kind() == ErrorKind::IsADirectory));
    }
}
