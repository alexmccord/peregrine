pub mod cursor;
pub mod lexer;
pub mod offside;
pub mod parser;

use lexer::tok::{Group, TokenVec};

use crate::ast::expr::ExprId;
use crate::ast::node::{Node, NodeId};
use crate::ast::{Ast, SourceSpan, TokenSpan};

pub use lexer::tokenize;
pub use parser::parse;

pub struct SourceModule {
    pub ast: Ast,
    pub tokens: TokenVec,
}

impl SourceModule {
    fn new(ast: Ast, tokens: TokenVec) -> SourceModule {
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
    pub errors: Vec<(NodeId, SyntaxError)>,
}

impl ParseResult {
    pub fn ast(&self) -> &Ast {
        &self.source_module.ast
    }

    pub fn tokens(&self) -> &TokenVec {
        &self.source_module.tokens
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    NotBalanced(Group),
    MissingName,
    MissingExpr,
    MissingDecl,
    LetDeclCannotHaveIn(Option<ExprId>),
    LetDeclIsLikelyMissingAnEquation,
    LetExprIsRequiredToHaveEquations,
    DoIsEmpty,
    LastStmtInDoMustBeExpr,
    IfExpr {
        missing_if_kw: bool,
        missing_then_kw: bool,
        missing_else_kw: bool,
    },
}
