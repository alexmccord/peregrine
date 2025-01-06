use crate::syntax::slab;

use super::cursor::Delimiter;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExprId(slab::SlabId);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DeclId(slab::SlabId);

#[derive(Default)]
pub struct AstAllocator {
    exprs: slab::Slab<Expr>,
    decls: slab::Slab<Decl>,
}

impl AstAllocator {
    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.insert(expr))
    }

    pub fn alloc_decl(&mut self, decl: Decl) -> DeclId {
        DeclId(self.decls.insert(decl))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AstError {
    MissingName,
    MissingExpr,
    Imbalanced(Delimiter),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Symbol(pub String);

#[derive(Debug)]
pub enum Expr {
    Var(Symbol),
    Lam(ExprId, ExprId),
    App(ExprId, ExprId),
    Ann(ExprId, ExprId),
    Num(String),
    Error(AstError, Option<ExprId>),
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
pub struct Struct(pub ExprId);

#[derive(Debug)]
pub struct Data(pub ExprId);

#[derive(Debug)]
pub enum Let {
    // let f : Nat -> Nat
    Decl(ExprId),
    // let x = 5
    DeclExpr(ExprId, ExprId),
    // let add2 x = x + 2 in add2 5
    DeclExprIn(ExprId, ExprId, ExprId),
}

#[derive(Debug)]
pub enum Decl {
    Module(Module),
    Import(Import),
    Struct(Struct),
    Data(Data),
    Let(Let),
    Error(AstError, Option<DeclId>),
}

impl Decl {
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

pub struct Program<'ast> {
    arena: &'ast AstAllocator,
    pub decls: Vec<DeclId>,
}

impl<'ast> Program<'ast> {
    pub fn new(arena: &'ast AstAllocator, decls: Vec<DeclId>) -> Program<'ast> {
        Program { arena, decls }
    }

    pub fn get_expr(&self, ExprId(id): ExprId) -> &Expr {
        self.arena.exprs.get(id)
    }

    pub fn get_decl(&self, DeclId(id): DeclId) -> &Decl {
        self.arena.decls.get(id)
    }
}
