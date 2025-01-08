use crate::syntax::ast;
use crate::syntax::lexer::{Keyword, Lexer, Token, TokenKind};

use super::ast::{AstAllocator, AstError, Program, Symbol};
use super::cursor::{Delimiter, Gate};

pub enum Precedence {
    TyAnn, // e : T
    App,   // f e
    Assoc, // (e)
}

pub struct Parser<'ast> {
    lexer: Lexer,
    arena: &'ast mut AstAllocator,
    lookahead: Option<Token>,
}

impl<'ast> Parser<'ast> {
    fn new(input: String, arena: &'ast mut AstAllocator) -> Parser<'ast> {
        Parser {
            lexer: Lexer::new(input),
            arena,
            lookahead: None,
        }
    }

    pub fn parse(input: String, arena: &'ast mut AstAllocator) -> Program<'ast> {
        let parser = Parser::new(input, arena);
        parser.parse_program()
    }

    fn next(&mut self) -> Option<Token> {
        // TODO: This function will skip comments and other random trivias.
        // Trivias are things that has to be emitted, but are otherwise
        // not significant with respect to the parser. Since Peregrine is
        // whitespace sensitive, indent/dedent/newlines are significant,
        // and thus are not trivias.
        if let Some(tok) = self.lookahead.take() {
            return Some(tok);
        }

        self.lexer.next()
    }

    fn try_consume<F>(&mut self, f: F) -> Option<Token>
    where
        F: FnOnce(&Token) -> bool,
    {
        if self.lookahead.is_none() {
            self.lookahead = self.lexer.next();
        }

        let Some(tok) = &self.lookahead else {
            return None;
        };

        if f(&tok) {
            self.lookahead.take()
        } else {
            None
        }
    }

    fn try_parse_kw(&mut self, kw: Keyword) -> Option<Token> {
        self.try_consume(|tok| tok.is_kw(kw))
    }

    fn try_parse_operator(&mut self, c: char) -> Option<Token> {
        self.try_consume(|tok| match &tok.kind {
            TokenKind::Operator(s) => s.as_bytes() == [c as u8],
            _ => false,
        })
    }

    fn try_parse_delimiter(&mut self, delim: Delimiter) -> Option<Token> {
        self.try_consume(|tok| match &tok.kind {
            TokenKind::Delimiter(d) => d == &delim,
            _ => false,
        })
    }

    fn make_err_expr(&mut self, err: AstError, expr: ast::ExprId) -> ast::ExprId {
        self.arena.alloc_expr(ast::Expr::Error(err, Some(expr)))
    }

    fn report_expr_err(&mut self, err: AstError) -> ast::ExprId {
        self.arena.alloc_expr(ast::Expr::Error(err, None))
    }

    fn make_err_decl(&mut self, err: AstError, decl: ast::DeclId) -> ast::DeclId {
        self.arena.alloc_decl(ast::Decl::Error(err, Some(decl)))
    }

    fn report_decl_error(&mut self, err: AstError) -> ast::DeclId {
        self.arena.alloc_decl(ast::Decl::Error(err, None))
    }

    fn make_expr(&mut self, expr: ast::Expr) -> ast::ExprId {
        self.arena.alloc_expr(expr)
    }

    fn make_decl(&mut self, decl: ast::Decl) -> ast::DeclId {
        self.arena.alloc_decl(decl)
    }

    fn parse_program(mut self) -> ast::Program<'ast> {
        let mut decls = Vec::new();

        while let Some(decl) = self.parse_top_level() {
            decls.push(decl);
        }

        ast::Program::new(self.arena, decls)
    }

    fn parse_top_level(&mut self) -> Option<ast::DeclId> {
        let tok = self.next()?;
        match tok.kind {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(Keyword::Module) => todo!(),
            TokenKind::Kw(Keyword::Import) => Some(self.parse_import_decl()),
            TokenKind::Kw(Keyword::Struct) => todo!(),
            TokenKind::Kw(Keyword::Data) => todo!(),
            TokenKind::Kw(Keyword::Let) => Some(self.parse_let_decl()),
            TokenKind::Kw(Keyword::Do) => todo!(),
            TokenKind::Kw(Keyword::In) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(_) => todo!(),
        }
    }

    fn parse_import_decl(&mut self) -> ast::DeclId {
        let mut path = Vec::new();

        loop {
            let tok = self.next();
            if let Some(ident) = tok.as_ref().and_then(Token::get_ident) {
                path.push(ident.clone());

                if self.try_parse_operator('.').is_none() {
                    break;
                }
            } else {
                let decl = self.make_decl(ast::Decl::Import(ast::Import { path }));
                return self.make_err_decl(AstError::MissingName, decl);
            }
        }

        self.make_decl(ast::Decl::Import(ast::Import { path }))
    }

    fn parse_let_decl(&mut self) -> ast::DeclId {
        let Some(f) = self.parse_expr(Precedence::TyAnn) else {
            return self.report_decl_error(AstError::MissingExpr);
        };

        if self.try_parse_operator('=').is_none() {
            return self.make_decl(ast::Decl::Let(ast::Let::Decl(f)));
        }

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(ast::Decl::Let(ast::Let::Decl(f)));
            return self.make_err_decl(AstError::MissingExpr, decl);
        };

        if self.try_parse_kw(Keyword::In).is_none() {
            return self.make_decl(ast::Decl::Let(ast::Let::DeclExpr(f, e)));
        };

        let Some(r) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(ast::Decl::Let(ast::Let::DeclExpr(f, e)));
            return self.make_err_decl(AstError::MissingExpr, decl);
        };

        self.make_decl(ast::Decl::Let(ast::Let::DeclExprIn(f, e, r)))
    }

    fn parse_expr(&mut self, pred: Precedence) -> Option<ast::ExprId> {
        match pred {
            Precedence::TyAnn => {
                let expr = self.parse_expr(Precedence::App);

                if self.try_parse_operator(':').is_none() {
                    return expr;
                };

                // Beautiful, beautiful Curry-Howard correspondence.
                // No need for a whole separate language for types. :)
                let ty = self.parse_expr(Precedence::App);

                match (expr, ty) {
                    (Some(expr), Some(ty)) => Some(self.make_expr(ast::Expr::Ann(expr, ty))),
                    (Some(expr), None) => {
                        let ty = self.report_expr_err(AstError::MissingExpr);
                        let ann = self.make_expr(ast::Expr::Ann(expr, ty));
                        Some(self.make_err_expr(AstError::MissingExpr, ann))
                    }
                    (None, Some(ty)) => {
                        let expr = self.report_expr_err(AstError::MissingExpr);
                        let ann = self.make_expr(ast::Expr::Ann(expr, ty));
                        Some(self.make_err_expr(AstError::MissingExpr, ann))
                    }
                    (None, None) => Some(self.report_expr_err(AstError::MissingExpr)),
                }
            }
            Precedence::App => {
                let tok = self.next()?;

                match tok.kind {
                    TokenKind::Unknown(_) => todo!(),
                    TokenKind::Kw(Keyword::Module) => todo!(),
                    TokenKind::Kw(Keyword::Import) => todo!(),
                    TokenKind::Kw(Keyword::Struct) => todo!(),
                    TokenKind::Kw(Keyword::Data) => todo!(),
                    TokenKind::Kw(Keyword::Let) => todo!(),
                    TokenKind::Kw(Keyword::Do) => todo!(),
                    TokenKind::Kw(Keyword::In) => todo!(),
                    TokenKind::Ident(ident) => {
                        let sym = Symbol(ident.to_string());
                        Some(self.make_expr(ast::Expr::Var(sym)))
                    }
                    TokenKind::Numeral(n) => {
                        let str = n.to_string();
                        Some(self.make_expr(ast::Expr::Num(str)))
                    }
                    TokenKind::Operator(_) => todo!(),
                    TokenKind::Delimiter(Delimiter::Paren(Gate::Opened)) => {
                        // Two missed opportunities to report errors.
                        let e = self.parse_expr(Precedence::Assoc);
                        self.try_parse_delimiter(Delimiter::Paren(Gate::Closed));
                        e
                    }
                    TokenKind::Delimiter(Delimiter::Brace(Gate::Opened)) => todo!(),
                    TokenKind::Delimiter(Delimiter::Bracket(Gate::Opened)) => todo!(),
                    TokenKind::Delimiter(
                        d @ (Delimiter::Paren(Gate::Closed)
                        | Delimiter::Brace(Gate::Closed)
                        | Delimiter::Bracket(Gate::Closed)),
                    ) => Some(self.report_expr_err(AstError::Imbalanced(d.clone()))),
                }
            }
            Precedence::Assoc => {
                let tok = self.next()?;

                match tok.kind {
                    TokenKind::Unknown(_) => todo!(),
                    TokenKind::Kw(_) => todo!(),
                    TokenKind::Ident(_) => todo!(),
                    TokenKind::Numeral(_) => todo!(),
                    TokenKind::Operator(_) => todo!(),
                    TokenKind::Delimiter(_) => todo!(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{AstAllocator, Parser};

    #[test]
    fn parse_nothing() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("".to_string(), &mut arena);
        assert!(result.decls.is_empty());
    }

    #[test]
    fn parse_import_decl() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("import A.B.C".to_string(), &mut arena);
        assert_eq!(result.decls.len(), 1);

        let import_decl_id = result.decls[0];
        let import_decl = result.get_decl(import_decl_id).get_import().unwrap();
        assert_eq!(import_decl.path, vec!["A", "B", "C"]);
    }

    #[test]
    fn parse_let_five_be_5() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("let five = 5".to_string(), &mut arena);
        assert_eq!(result.decls.len(), 1);

        let let_decl_id = result.decls[0];
    }

    #[test]
    fn parse_let_paren_x_paren_be_2() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("let (x) = 2".to_string(), &mut arena);
        assert_eq!(result.decls.len(), 1);

        let let_decl_id = result.decls[0];
        let let_decl = result.get_decl(let_decl_id).get_let().unwrap();
    }

    #[test]
    fn parse_let_id_which_is_a_to_a() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("let id : a -> a".to_string(), &mut arena);
        assert_eq!(result.decls.len(), 1);

        let let_decl_id = result.decls[0];
        let let_decl = result.get_decl(let_decl_id).get_let().unwrap();
    }

    #[test]
    fn parse_let_id_x_be_x() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("let id x = x".to_string(), &mut arena);
        assert_eq!(result.decls.len(), 1);

        let let_decl_id = result.decls[0];
        let let_decl = result.get_decl(let_decl_id).get_let().unwrap();
    }
}
