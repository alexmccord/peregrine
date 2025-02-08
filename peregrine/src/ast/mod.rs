pub mod decl;
pub mod expr;
pub mod node;

use decl::Decl;

use crate::syn::lexer::TokenId;

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

#[derive(Debug, Clone)]
pub struct SourceSpan {
    pub begin: Position,
    pub end: Position,
}

impl SourceSpan {
    pub fn new(begin: Position, end: Position) -> SourceSpan {
        SourceSpan { begin, end }
    }
}

#[derive(Debug, Clone)]
pub struct TokenSpan {
    pub begin: TokenId,
    pub end: TokenId,
}

impl TokenSpan {
    pub fn new(begin: TokenId, end: TokenId) -> TokenSpan {
        TokenSpan { begin, end }
    }
}
