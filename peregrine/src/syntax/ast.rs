use super::cursor::Delimiter;

pub type ExprId = id_arena::Id<Expr>;
pub type DeclId = id_arena::Id<Decl>;

#[derive(Default)]
pub(crate) struct AstAllocator {
    exprs: id_arena::Arena<Expr>,
    decls: id_arena::Arena<Decl>,
}

impl AstAllocator {
    pub(crate) fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        self.exprs.alloc(expr)
    }

    pub(crate) fn alloc_decl(&mut self, decl: Decl) -> DeclId {
        self.decls.alloc(decl)
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

pub struct Program {
    arena: Box<AstAllocator>,
    pub decls: Vec<DeclId>,
}

impl Program {
    pub fn new(arena: AstAllocator, decls: Vec<DeclId>) -> Program {
        Program {
            arena: Box::new(arena),
            decls,
        }
    }

    pub fn get_expr(&self, id: ExprId) -> &Expr {
        &self.arena.exprs[id]
    }

    pub fn get_decl(&self, id: DeclId) -> &Decl {
        &self.arena.decls[id]
    }
}
