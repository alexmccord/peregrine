use std::path::PathBuf;

use crate::syntax::ast::AstAllocator;
use crate::syntax::parser::Parser;

struct BuildDriver {
    pending: Vec<PathBuf>,
}

impl BuildDriver {
    pub fn new() -> BuildDriver {
        BuildDriver {
            pending: Vec::default(),
        }
    }
}
