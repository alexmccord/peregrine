use crate::idx;

use crate::ast::expr::Expr;
use crate::ast::TokenSpan;

use crate::syn::lexer::tok::TokenId;

idx::newindex!(pub DeclId);

#[derive(Debug)]
pub struct Decl {
    id: DeclId,
    kind: DeclKind,
    token_span: TokenSpan,
}

#[derive(Debug)]
pub enum DeclKind {
    // module Foo
    Module(Module),
    // import Foo.Bar
    Import(Import),
    // export <decl>
    Export(Export),
    // pub <decl>
    Public(Public),
    // open <decl>
    Open(Open),
    // struct Foo where
    //   a : A
    //   b : B
    Struct(Struct),
    // data Vec n a where
    //   []   : Vec 0 a
    //   (::) : a -> Vec n a -> Vec (n + 1) a
    Data(Data),
    // class Functor f where
    //   let map : (a -> b) -> f a -> f b
    Class(Class),
    // instance Functor Maybe where
    //   let map f = function
    //     | Just x  -> Just (f x)
    //     | Nothing -> Nothing
    Instance(Instance),
    // let e : T
    // let e = x
    Let(Let),
    // In REPL mode only:
    Expr(Expr),
    Error(Option<DeclId>),
}

#[derive(Debug)]
pub struct Module {
    path: Option<Vec<String>>,
    decls: Vec<Decl>,
}

#[derive(Debug)]
pub struct Import {
    path: Vec<String>,
}

#[derive(Debug)]
pub struct Export(pub(crate) Box<Decl>); // TODO: export (..)

#[derive(Debug)]
pub struct Public(pub(crate) Box<Decl>);

#[derive(Debug)]
pub struct Open(pub(crate) Box<Decl>);

#[derive(Debug)]
pub struct Struct(pub(crate) Expr, pub(crate) Vec<Expr>);

#[derive(Debug)]
pub struct Data(
    pub(crate) Expr,
    pub(crate) DataCons,
    pub(crate) Option<Deriving>,
);

#[derive(Debug)]
pub enum DataCons {
    NoCons,
    Equals(Expr),
    Where(Vec<Expr>),
}

#[derive(Debug)]
pub struct Deriving(pub Expr);

#[derive(Debug)]
pub struct Class(pub(crate) Expr, pub(crate) Vec<Expr>);

#[derive(Debug)]
pub struct Instance(pub(crate) Expr, pub(crate) Vec<Expr>);

// TODO: Rework top-level `let`.
//
// Originally, I was thinking to have OCaml style `let` but ran into some annoyances
// where `=` is ambiguous in some cases such as:
//
// let plus_comm : (x : Nat) -> (y : Nat) -> x + y = y + x = Refl
//
// The problem there is, how do you determine if `Refl` is:
// 1. the result of `plus_comm` where applying `plus_comm` gives the proof `Refl`?
// 2. a transitive equality, similar to `g . f = id = f . g`?
//
// So I decided to go with the Haskell syntax, after all:
//
// plus_comm : (x : Nat) -> (y : Nat) -> x + y = y + x
// plus_comm = Refl
//
// Am I saddened by it? A bit, yes. But also, not really. I've realized that the
// `let` spam would get annoying fast in a functional programming language, so...
// it's a net win, I guess. Consider the alternatives.
//
// 1) Keep the `let` but split it across lines.
//    let plus_comm : (x : Nat) -> (y : Nat) -> x + y = y + x
//    let plus_comm = Refl
//
// 2) Keep the `let` and use layout rule:
//    let plus_comm : (x : Nat) -> (y : Nat) -> x + y = y + x
//        plus_comm = Refl
//
// 3) Overengineer it:
//    pub let new : [Char] -> String where
//      new []      = ""
//      new (c::cs) = "{c}{new cs}"
//
// Neither is great and feels like the `let` is superfluous so I'm just going to
// drop the `let` keyword. It does have implications for the module system in nonobvious
// ways, so I'll take the time to reconsider things carefully. I was okay with this:
//
// pub let new : [Char] -> String =
//   function
//   | []      -> ""
//   | (c::cs) -> "{c}{new cs}"
//
// But not this where the function name and the equations for them are out of alignment:
//
// pub new : [Char] -> String
// new []      = ""
// new (c::cs) = "{c}{new cs}"
//
// Even if you had `pub` on a line of its own.
//
// pub
// new : [Char] -> String
// new []      = ""
// new (c::cs) = "{c}{new cs}"
//
// So, need to give the module system more thinking in the face of our new reality.
#[derive(Debug)]
pub enum Let {
    Decl(Expr),
    DeclExpr(Expr, Expr),
}

impl Decl {
    pub fn new(id: DeclId, kind: DeclKind, token_span: TokenSpan) -> Decl {
        Decl {
            id,
            kind,
            token_span,
        }
    }

    pub fn id(&self) -> DeclId {
        self.id
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn token_span(&self) -> TokenSpan {
        self.token_span.clone()
    }

    pub fn begin_token(&self) -> TokenId {
        self.token_span.begin
    }

    pub fn end_token(&self) -> TokenId {
        self.token_span.end
    }

    pub fn as_module(&self) -> Option<&Module> {
        match self.kind() {
            DeclKind::Module(m) => Some(m),
            _ => None,
        }
    }

    pub fn as_import(&self) -> Option<&Import> {
        match self.kind() {
            DeclKind::Import(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_export(&self) -> Option<&Export> {
        match self.kind() {
            DeclKind::Export(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_public(&self) -> Option<&Public> {
        match self.kind() {
            DeclKind::Public(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_open(&self) -> Option<&Open> {
        match self.kind() {
            DeclKind::Open(o) => Some(o),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&Struct> {
        match self.kind() {
            DeclKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_data(&self) -> Option<&Data> {
        match self.kind() {
            DeclKind::Data(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self.kind() {
            DeclKind::Class(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_instance(&self) -> Option<&Instance> {
        match self.kind() {
            DeclKind::Instance(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_let(&self) -> Option<&Let> {
        match self.kind() {
            DeclKind::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_expr(&self) -> Option<&Expr> {
        match self.kind() {
            DeclKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_error(&self) -> Option<Option<DeclId>> {
        match self.kind() {
            DeclKind::Error(e) => Some(e.clone()),
            _ => None,
        }
    }
}

impl DeclKind {
    pub fn module(path: Option<Vec<String>>, decls: Vec<Decl>) -> DeclKind {
        DeclKind::Module(Module::new(path, decls))
    }

    pub fn import(path: Vec<String>) -> DeclKind {
        DeclKind::Import(Import::new(path))
    }

    pub fn export(decl: Decl) -> DeclKind {
        DeclKind::Export(Export::new(decl))
    }

    pub fn public(decl: Decl) -> DeclKind {
        DeclKind::Public(Public::new(decl))
    }
    pub fn open(decl: Decl) -> DeclKind {
        DeclKind::Open(Open::new(decl))
    }

    pub fn structure(sig: Expr, fields: Vec<Expr>) -> DeclKind {
        DeclKind::Struct(Struct::new(sig, fields))
    }

    pub fn data(sig: Expr, cons: DataCons, deriving: Option<Deriving>) -> DeclKind {
        DeclKind::Data(Data::new(sig, cons, deriving))
    }

    pub fn empty(sig: Expr) -> DeclKind {
        DeclKind::Data(Data::empty(sig))
    }

    pub fn adt(sig: Expr, constructors: Expr, deriving: Option<Deriving>) -> DeclKind {
        DeclKind::Data(Data::adt(sig, constructors, deriving))
    }

    pub fn gadt(sig: Expr, constructors: Vec<Expr>, deriving: Option<Deriving>) -> DeclKind {
        DeclKind::Data(Data::gadt(sig, constructors, deriving))
    }

    pub fn class(sig: Expr, body: Vec<Expr>) -> DeclKind {
        DeclKind::Class(Class::new(sig, body))
    }

    pub fn instance(sig: Expr, body: Vec<Expr>) -> DeclKind {
        DeclKind::Instance(Instance::new(sig, body))
    }

    pub fn let_the_expr(the: Expr) -> DeclKind {
        DeclKind::Let(Let::the_expr(the))
    }

    pub fn let_the_expr_be(the: Expr, be: Expr) -> DeclKind {
        DeclKind::Let(Let::the_expr_be(the, be))
    }

    pub fn expr(expr: Expr) -> DeclKind {
        DeclKind::Expr(expr)
    }
}

impl Module {
    pub fn new(path: Option<Vec<String>>, decls: Vec<Decl>) -> Module {
        Module { path, decls }
    }

    pub fn path(&self) -> Option<&Vec<String>> {
        self.path.as_ref()
    }

    pub fn decls(&self) -> &Vec<Decl> {
        &self.decls
    }
}

impl Import {
    pub fn new(path: Vec<String>) -> Import {
        Import { path }
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }
}

impl Export {
    pub fn new(decl: Decl) -> Export {
        Export(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Public {
    pub fn new(decl: Decl) -> Public {
        Public(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Open {
    pub fn new(decl: Decl) -> Open {
        Open(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Struct {
    pub fn new(sig: Expr, fields: Vec<Expr>) -> Struct {
        Struct(sig, fields)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn fields(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Data {
    pub fn new(sig: Expr, cons: DataCons, deriving: Option<Deriving>) -> Data {
        Data(sig, cons, deriving)
    }

    pub fn empty(sig: Expr) -> Data {
        Data(sig, DataCons::NoCons, None)
    }

    pub fn adt(sig: Expr, constructors: Expr, deriving: Option<Deriving>) -> Data {
        Data(sig, DataCons::Equals(constructors), deriving)
    }

    pub fn gadt(sig: Expr, constructors: Vec<Expr>, deriving: Option<Deriving>) -> Data {
        Data(sig, DataCons::Where(constructors), deriving)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn cons(&self) -> &DataCons {
        &self.1
    }

    pub fn deriving(&self) -> Option<&Deriving> {
        self.2.as_ref()
    }
}

impl Class {
    pub fn new(sig: Expr, body: Vec<Expr>) -> Class {
        Class(sig, body)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn body(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Instance {
    pub fn new(sig: Expr, body: Vec<Expr>) -> Instance {
        Instance(sig, body)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn body(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Let {
    pub fn the_expr(the: Expr) -> Let {
        Let::Decl(the)
    }

    pub fn the_expr_be(the: Expr, be: Expr) -> Let {
        Let::DeclExpr(the, be)
    }

    pub fn get_the_expr(&self) -> &Expr {
        match self {
            Let::Decl(the) => &the,
            Let::DeclExpr(the, _) => &the,
        }
    }

    pub fn get_be_expr(&self) -> Option<&Expr> {
        match self {
            Let::Decl(_) => None,
            Let::DeclExpr(_, be) => Some(&be),
        }
    }
}
