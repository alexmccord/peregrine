use crate::syntax::slab;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExprId<'ast>(slab::SlabId<'ast>);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DeclId<'ast>(slab::SlabId<'ast>);

#[derive(Default)]
pub struct AstAllocator<'ast> {
    exprs: slab::Slab<'ast, Expr<'ast>>,
    decls: slab::Slab<'ast, Decl<'ast>>,
}

impl<'ast> AstAllocator<'ast> {
    pub fn alloc_expr(&mut self, expr: Expr<'ast>) -> ExprId<'ast> {
        ExprId(self.exprs.insert(expr))
    }

    pub fn alloc_decl(&mut self, decl: Decl<'ast>) -> DeclId<'ast> {
        DeclId(self.decls.insert(decl))
    }

    pub fn get_expr(&self, ExprId(id): ExprId<'ast>) -> &Expr<'ast> {
        self.exprs.get(id)
    }

    pub fn get_decl(&self, DeclId(id): DeclId<'ast>) -> &Decl<'ast> {
        self.decls.get(id)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Symbol(pub String);

#[derive(Debug)]
pub enum Expr<'ast> {
    Var(Symbol),
    Lam(ExprId<'ast>, ExprId<'ast>),
    App(ExprId<'ast>, ExprId<'ast>),
    Ann(ExprId<'ast>, ExprId<'ast>),
    Error,
}

#[derive(Debug)]
pub struct Module {
    pub path: Vec<String>,
}

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
}

// struct e where
// struct e : T where
#[derive(Debug)]
pub struct Struct<'ast>(pub ExprId<'ast>);

#[derive(Debug)]
pub struct Data<'ast>(pub ExprId<'ast>);

#[derive(Debug)]
pub enum Let<'ast> {
    // let f : Nat -> Nat
    Decl(ExprId<'ast>),
    // let x = 5
    Assign(ExprId<'ast>, ExprId<'ast>),
    // let add2 x = x + 2 in add2 5
    Scoped(ExprId<'ast>, ExprId<'ast>, ExprId<'ast>),
}

#[derive(Debug)]
pub enum Decl<'ast> {
    Module(Module),
    Import(Import),
    Struct(Struct<'ast>),
    Data(Data<'ast>),
    Let(Let<'ast>),
}

impl<'ast> Decl<'ast> {
    pub fn get_module(&self) -> Option<&Module> {
        match self {
            Decl::Module(module) => Some(module),
            _ => None,
        }
    }

    pub fn get_import(&self) -> Option<&Import> {
        match self {
            Decl::Import(import) => Some(import),
            _ => None,
        }
    }

    pub fn get_struct(&self) -> Option<&Struct> {
        match self {
            Decl::Struct(strukt) => Some(strukt),
            _ => None,
        }
    }

    pub fn get_data(&self) -> Option<&Data> {
        match self {
            Decl::Data(data) => Some(data),
            _ => None,
        }
    }

    pub fn get_let(&self) -> Option<&Let> {
        match self {
            Decl::Let(l) => Some(l),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Program<'ast> {
    pub decls: Vec<DeclId<'ast>>,
}

impl<'ast> Program<'ast> {
    pub fn new() -> Program<'ast> {
        Program { decls: Vec::new() }
    }

    pub fn push(&mut self, decl: DeclId<'ast>) {
        self.decls.push(decl)
    }
}
