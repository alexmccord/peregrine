use std::collections::VecDeque;
use std::path::PathBuf;

use crate::syntax::ast::Program;
use crate::syntax::parser::Parser;

pub enum BuildTask {
    Project(PathBuf),
}

pub struct BuildDriver {
    pending: VecDeque<BuildTask>,
}

impl BuildDriver {
    pub fn new() -> BuildDriver {
        BuildDriver {
            pending: VecDeque::default(),
        }
    }

    pub fn submit_task(&mut self, task: BuildTask) {
        self.pending.push_back(task);
    }

    pub fn execute(&mut self) {
        while let Some(task) = self.pending.pop_front() {
            match task {
                BuildTask::Project(path) => self.build_project(path),
            }
        }
    }

    fn build_project(&mut self, path: PathBuf) {}
}
