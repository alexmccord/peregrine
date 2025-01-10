use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;

use crate::syntax::ast::Program;
use crate::syntax::parser::Parser;

#[derive(Debug)]
pub enum Task {
    SetupProject(PathBuf),
    SetupModule(PathBuf),
}

pub enum BuildStatus {
    Parsed,
}

struct Module {
    program: Program,
    status: BuildStatus,
}

impl Module {
    fn new(program: Program) -> Module {
        Module {
            program,
            status: BuildStatus::Parsed,
        }
    }
}

pub struct BuildDriver {
    pending: VecDeque<Task>,
    modules: HashMap<PathBuf, Module>,
}

impl BuildDriver {
    pub fn new() -> BuildDriver {
        BuildDriver {
            pending: VecDeque::default(),
            modules: HashMap::default(),
        }
    }

    pub fn submit_task(&mut self, task: Task) {
        self.pending.push_back(task);
    }

    pub fn execute(&mut self) {
        while let Some(task) = self.pending.pop_front() {
            match task {
                Task::SetupProject(path) => self.setup_project(path),
                Task::SetupModule(path) => self.setup_module(path),
            }
        }
    }

    fn setup_project(&mut self, path: PathBuf) {
        self.submit_task(Task::SetupModule(path));
    }

    fn setup_module(&mut self, path: PathBuf) {
        if !path.is_file() || !path.ends_with(".prg") {
            return;
        }

        let content = std::fs::read_to_string(path.clone()).unwrap();
        let result = Parser::parse(content);

        self.modules.insert(path, Module::new(result));
    }
}
