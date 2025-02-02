pub mod decl;
pub mod expr;
pub mod node;

use decl::Decl;

pub struct Ast {
    decls: Vec<Decl>,
}

impl Ast {
    pub(crate) fn new(decls: Vec<Decl>) -> Ast {
        Ast { decls }
    }

    pub fn decls(&self) -> &Vec<Decl> {
        &self.decls
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    /// Line begins at 1.
    pub fn line(self) -> usize {
        self.line
    }

    /// Column begins at 0.
    pub fn column(self) -> usize {
        self.column
    }

    pub(crate) fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
}

#[derive(Debug)]
pub struct Span {
    pub begin: Position,
    pub end: Position,
}

impl Span {
    pub fn new(begin: Position, end: Position) -> Span {
        Span { begin, end }
    }
}
