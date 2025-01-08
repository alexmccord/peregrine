use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;

use crate::syntax::ast::{AstAllocator, DeclId, ExprId, Program};
use crate::syntax::parser::Parser;

#[derive(Debug)]
pub enum BuildTask {
    Project(PathBuf),
}

#[derive(Default)]
pub struct Analysis<T> {
    decls: HashMap<DeclId, T>,
    exprs: HashMap<ExprId, T>,
}

struct Module<'ast> {
    program: Program<'ast>,
}

impl<'ast> Module<'ast> {
    fn new(program: Program<'ast>) -> Module<'ast> {
        Module { program }
    }
}

pub struct BuildDriver<'ast> {
    pending: VecDeque<BuildTask>,
    allocators: HashMap<PathBuf, AstAllocator>,
    modules: HashMap<PathBuf, Module<'ast>>,
}

impl<'ast> BuildDriver<'ast> {
    pub fn new() -> BuildDriver<'ast> {
        BuildDriver {
            pending: VecDeque::default(),
            allocators: HashMap::default(),
            modules: HashMap::default(),
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

    fn build_project(&mut self, path: PathBuf) {
        //
    }

    fn build_module(&'ast mut self, path: PathBuf) {
        if !path.is_file() || !path.ends_with(".prg") {
            return;
        }

        let entry = self
            .allocators
            .entry(path.clone())
            .or_insert(AstAllocator::default());

        let content = std::fs::read_to_string(path.clone()).unwrap();
        let result = Parser::parse(content, entry);

        self.modules.insert(path, Module::new(result));
    }
}
