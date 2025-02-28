use crate::ast::expr::ExprId;
use crate::ast::node::Node;
use crate::ast::{Ast, SourceSpan, TokenSpan};

mod cursor;
mod lexer;
mod offside;
mod parser;

pub use lexer::*;
pub use parser::*;

pub struct SourceModule {
    pub ast: Ast,
    pub tokens: tok::TokenVec,
}

impl SourceModule {
    fn new(ast: Ast, tokens: tok::TokenVec) -> SourceModule {
        SourceModule { ast, tokens }
    }

    pub fn token_span<'a>(&self, node: impl Into<Node<'a>>) -> TokenSpan {
        match node.into() {
            Node::Expr(e) => e.token_span(),
            Node::Decl(d) => d.token_span(),
        }
    }

    pub fn source_span<'a>(&self, node: impl Into<Node<'a>>) -> SourceSpan {
        let TokenSpan { begin, end } = self.token_span(node);
        SourceSpan::new(self.tokens.get_pos(begin).0, self.tokens.get_pos(end).1)
    }
}

pub struct ParseResult {
    pub source_module: SourceModule,
    pub errors: Vec<SyntaxError>,
}

impl ParseResult {
    pub fn ast(&self) -> &Ast {
        &self.source_module.ast
    }

    pub fn tokens(&self) -> &tok::TokenVec {
        &self.source_module.tokens
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    NotBalanced(tok::Group),
    UnknownToken(tok::TokenId),
    ExpectedTokenKind(tok::TokenKind),
    ExpectedTokenType(tok::TokenType),
    MissingName,
    MissingExpr,
    MissingDecl,
    LetDeclCannotHaveIn(Option<ExprId>),
    LetDeclIsLikelyMissingAnEquation,
    LetExprIsRequiredToHaveEquations,
    DoIsEmpty,
    LastStmtInDoMustBeExpr,
}
