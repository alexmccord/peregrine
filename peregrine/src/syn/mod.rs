pub mod cursor;
pub mod lexer;
pub mod parser;

use cursor::Delimiter;
use lexer::Token;
use parser::Parser;

use crate::ast::expr::ExprId;
use crate::ast::node::NodeId;
use crate::ast::Ast;
use crate::idx;

pub struct ParseResult {
    pub ast: Ast,
    pub errors: Vec<(NodeId, SyntaxError)>,
    pub tokens: idx::IndexedVec<Token>,
}

pub fn parse(input: impl Into<String>) -> ParseResult {
    Parser::parse(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    NotBalanced(Delimiter),
    MissingName,
    MissingExpr,
    MissingDecl,
    LetDeclCannotHaveIn(Option<ExprId>),
    LetDeclIsLikelyMissingAnEquation,
    LetExprIsRequiredToHaveEquations,
    IfExpr {
        missing_if_kw: bool,
        missing_then_kw: bool,
        missing_else_kw: bool,
    },
}
