use std::slice::Iter;

use crate::syntax::cursor::Delimiter;

pub enum AstNode<'ast> {
    Expr(&'ast Expr),
    Decl(&'ast Decl),
}

// Rust's "automatic Deref" can bite me.
impl<'ast> From<&'ast Expr> for AstNode<'ast> {
    fn from(value: &'ast Expr) -> Self {
        AstNode::Expr(value)
    }
}

// Rust's "automatic Deref" can bite me.
impl<'ast> From<&'ast Decl> for AstNode<'ast> {
    fn from(value: &'ast Decl) -> Self {
        AstNode::Decl(value)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AstId {
    ExprId(ExprId),
    DeclId(DeclId),
}

impl PartialEq<ExprId> for AstId {
    fn eq(&self, other: &ExprId) -> bool {
        match (self, other) {
            (Self::ExprId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialEq<DeclId> for AstId {
    fn eq(&self, other: &DeclId) -> bool {
        match (self, other) {
            (Self::DeclId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

// Peregrine is gonna have symmetric type classes. -_-
impl PartialEq<AstId> for ExprId {
    fn eq(&self, other: &AstId) -> bool {
        other == self
    }
}

// Peregrine is gonna have symmetric type classes. -_-
impl PartialEq<AstId> for DeclId {
    fn eq(&self, other: &AstId) -> bool {
        other == self
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ExprId(id_arena::Id<Expr>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct DeclId(id_arena::Id<Decl>);

// Peregrine is gonna have transitive type classes. -_-
// Conversion from `id_arena::Id<Expr>` to `ExprId`,
// and then from `ExprId` to `AstId` should not require
// two `.into()` calls.
//
// I can't be bothered.
impl From<ExprId> for AstId {
    fn from(value: ExprId) -> Self {
        AstId::ExprId(value)
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&ExprId> for AstId {
    fn from(value: &ExprId) -> Self {
        value.clone().into()
    }
}

impl From<DeclId> for AstId {
    fn from(value: DeclId) -> Self {
        AstId::DeclId(value)
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&DeclId> for AstId {
    fn from(value: &DeclId) -> Self {
        value.clone().into()
    }
}

#[derive(Default)]
pub struct AstAllocator {
    exprs: id_arena::Arena<Expr>,
    decls: id_arena::Arena<Decl>,
}

impl AstAllocator {
    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }

    pub fn alloc_decl(&mut self, decl: Decl) -> DeclId {
        DeclId(self.decls.alloc(decl))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AstError {
    MissingName,
    MissingExpr,
    Imbalanced(Delimiter),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Var(pub String);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Num(pub String);

#[derive(Debug)]
pub enum Expr {
    Var(Var),
    Lam(ExprId, ExprId),
    App(ExprId, ExprId),
    Ann(ExprId, ExprId),
    Num(Num),
    Error(AstError, Option<ExprId>),
}

#[derive(Debug)]
pub struct Module {
    pub path: Option<Vec<String>>,
    pub decls: Vec<DeclId>,
}

impl Module {
    pub fn new(path: Option<Vec<String>>, decls: Vec<DeclId>) -> Module {
        Module { path, decls }
    }
}

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
}

// struct e where
// struct e : T where
#[derive(Debug)]
pub struct Struct(pub ExprId, pub StructFields);

#[derive(Debug)]
pub struct StructFields(pub Vec<ExprId>);

#[derive(Debug)]
pub struct Data(pub ExprId, pub DataCons);

#[derive(Debug)]
pub enum DataCons {
    NoCons,
    Equals(ExprId),
    Where(Vec<ExprId>),
}

#[derive(Debug)]
pub enum Let {
    Decl(ExprId),
    DeclExpr(ExprId, ExprId),
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

pub struct Ast {
    arena: AstAllocator,
    root: DeclId,
}

impl Ast {
    pub fn new(arena: AstAllocator, root: DeclId) -> Ast {
        Ast { arena, root }
    }

    pub fn get_ast(&self, id: AstId) -> AstNode {
        match id {
            AstId::ExprId(expr_id) => self.get_expr(expr_id).into(),
            AstId::DeclId(decl_id) => self.get_decl(decl_id).into(),
        }
    }

    pub fn get_expr(&self, ExprId(id): ExprId) -> &Expr {
        &self.arena.exprs[id]
    }

    pub fn get_decl(&self, DeclId(id): DeclId) -> &Decl {
        &self.arena.decls[id]
    }

    pub fn get_root(&self) -> DeclId {
        self.root
    }

    pub fn decls(&self) -> Iter<DeclId> {
        let Some(module) = self.get_decl(self.root).get_module() else {
            panic!("root is not a module?");
        };

        module.decls.iter()
    }
}
