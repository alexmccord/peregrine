use crate::syntax::ast::Ast;

pub struct Bag {}

impl Bag {
    fn new() -> Bag {
        Bag {}
    }
}

pub struct ModuleCtxt {
    ast: Ast,
    bag: Bag,
}

impl ModuleCtxt {
    pub fn new(ast: Ast) -> ModuleCtxt {
        ModuleCtxt {
            ast,
            bag: Bag::new(),
        }
    }
}
