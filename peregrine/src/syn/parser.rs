use crate::syn::ast::*;
use crate::syn::cursor::{Delimiter, Gate};
use crate::syn::lexer::{Keyword, Lexer, Token, TokenKind};

pub fn parse(input: impl Into<String>) -> Ast {
    let mut parser = Parser::new(input);
    let module = parser.parse_module();
    Ast::new(parser.arena, module)
}

pub enum Precedence {
    TyAnn, // e : T
    App,   // f e
    Assoc, // (e)
}

pub struct Parser {
    lexer: Lexer,
    arena: AstAllocator,
    lookahead: Option<Token>,
}

impl Parser {
    fn new(input: impl Into<String>) -> Parser {
        Parser {
            lexer: Lexer::new(input),
            arena: AstAllocator::default(),
            lookahead: None,
        }
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

    fn make_err_expr(&mut self, err: AstError, expr: ExprId) -> ExprId {
        self.arena.alloc_expr(Expr::Error(err, Some(expr)))
    }

    fn report_expr_err(&mut self, err: AstError) -> ExprId {
        self.arena.alloc_expr(Expr::Error(err, None))
    }

    fn make_err_decl(&mut self, err: AstError, decl: DeclId) -> DeclId {
        self.arena.alloc_decl(Decl::Error(err, Some(decl)))
    }

    fn report_decl_error(&mut self, err: AstError) -> DeclId {
        self.arena.alloc_decl(Decl::Error(err, None))
    }

    fn make_expr(&mut self, expr: Expr) -> ExprId {
        self.arena.alloc_expr(expr)
    }

    fn make_decl(&mut self, decl: Decl) -> DeclId {
        self.arena.alloc_decl(decl)
    }

    fn parse_module(&mut self) -> DeclId {
        if self.try_parse_kw(Keyword::Module).is_some() {
            self.parse_module_tail()
        } else {
            let decls = self.parse_decls();
            self.make_decl(Decl::Module(Module::new(None, decls)))
        }
    }

    fn parse_module_tail(&mut self) -> DeclId {
        let (ok, path) = self.parse_path();

        let decls = self.parse_decls();

        let decl = self.make_decl(Decl::Module(Module::new(Some(path), decls)));

        if ok {
            decl
        } else {
            self.make_err_decl(AstError::MissingName, decl)
        }
    }

    fn parse_decls(&mut self) -> Vec<DeclId> {
        let mut decls = Vec::new();

        while let Some(decl) = self.parse_decl() {
            decls.push(decl);
        }

        decls
    }

    fn parse_decl(&mut self) -> Option<DeclId> {
        let tok = self.next()?;
        match tok.kind {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(Keyword::Module) => Some(self.parse_module_tail()),
            TokenKind::Kw(Keyword::Import) => Some(self.parse_import_decl()),
            TokenKind::Kw(Keyword::Export) => todo!(),
            TokenKind::Kw(Keyword::Pub) => todo!(),
            TokenKind::Kw(Keyword::Open) => todo!(),
            TokenKind::Kw(Keyword::Hiding) => todo!(),
            TokenKind::Kw(Keyword::Renaming) => todo!(),
            TokenKind::Kw(Keyword::Struct) => todo!(),
            TokenKind::Kw(Keyword::Data) => todo!(),
            TokenKind::Kw(Keyword::Deriving) => todo!(),
            TokenKind::Kw(Keyword::Class) => todo!(),
            TokenKind::Kw(Keyword::Instance) => todo!(),
            TokenKind::Kw(Keyword::Where) => todo!(),
            TokenKind::Kw(Keyword::Let) => Some(self.parse_let_decl()),
            TokenKind::Kw(Keyword::In) => todo!(),
            TokenKind::Kw(Keyword::Do) => todo!(),
            TokenKind::Kw(Keyword::If) => todo!(),
            TokenKind::Kw(Keyword::Then) => todo!(),
            TokenKind::Kw(Keyword::Else) => todo!(),
            TokenKind::Kw(Keyword::Function) => todo!(),
            TokenKind::Kw(Keyword::Match) => todo!(),
            TokenKind::Kw(Keyword::With) => todo!(),
            TokenKind::Kw(Keyword::Forall) => todo!(),
            TokenKind::Kw(Keyword::Exists) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(_) => todo!(),
        }
    }

    fn parse_path(&mut self) -> (bool, Vec<String>) {
        let mut ok = true;
        let mut path = Vec::new();

        loop {
            let tok = self.next();
            if let Some(ident) = tok.as_ref().and_then(Token::get_ident) {
                path.push(ident.clone());

                if self.try_parse_operator('.').is_none() {
                    break;
                }
            } else {
                ok = false;
            }
        }

        (ok, path)
    }

    fn parse_import_decl(&mut self) -> DeclId {
        let (ok, path) = self.parse_path();
        let decl = self.make_decl(Decl::Import(Import { path }));

        if ok {
            decl
        } else {
            self.make_err_decl(AstError::MissingName, decl)
        }
    }

    fn parse_let_decl(&mut self) -> DeclId {
        let Some(f) = self.parse_expr(Precedence::TyAnn) else {
            return self.report_decl_error(AstError::MissingExpr);
        };

        if self.try_parse_operator('=').is_none() {
            return self.make_decl(Decl::Let(Let::Decl(f)));
        }

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(Decl::Let(Let::Decl(f)));
            return self.make_err_decl(AstError::MissingExpr, decl);
        };

        if self.try_parse_kw(Keyword::In).is_none() {
            return self.make_decl(Decl::Let(Let::DeclExpr(f, e)));
        };

        let Some(r) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(Decl::Let(Let::DeclExpr(f, e)));
            return self.make_err_decl(AstError::MissingExpr, decl);
        };

        self.make_decl(Decl::Let(Let::DeclExprIn(f, e, r)))
    }

    fn parse_expr(&mut self, pred: Precedence) -> Option<ExprId> {
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
                    (Some(expr), Some(ty)) => Some(self.make_expr(Expr::Ann(expr, ty))),
                    (Some(expr), None) => {
                        let ty = self.report_expr_err(AstError::MissingExpr);
                        let ann = self.make_expr(Expr::Ann(expr, ty));
                        Some(self.make_err_expr(AstError::MissingExpr, ann))
                    }
                    (None, Some(ty)) => {
                        let expr = self.report_expr_err(AstError::MissingExpr);
                        let ann = self.make_expr(Expr::Ann(expr, ty));
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
                    TokenKind::Kw(Keyword::Export) => todo!(),
                    TokenKind::Kw(Keyword::Pub) => todo!(),
                    TokenKind::Kw(Keyword::Open) => todo!(),
                    TokenKind::Kw(Keyword::Hiding) => todo!(),
                    TokenKind::Kw(Keyword::Renaming) => todo!(),
                    TokenKind::Kw(Keyword::Struct) => todo!(),
                    TokenKind::Kw(Keyword::Data) => todo!(),
                    TokenKind::Kw(Keyword::Deriving) => todo!(),
                    TokenKind::Kw(Keyword::Class) => todo!(),
                    TokenKind::Kw(Keyword::Instance) => todo!(),
                    TokenKind::Kw(Keyword::Where) => todo!(),
                    TokenKind::Kw(Keyword::Let) => todo!(),
                    TokenKind::Kw(Keyword::In) => todo!(),
                    TokenKind::Kw(Keyword::Do) => todo!(),
                    TokenKind::Kw(Keyword::If) => todo!(),
                    TokenKind::Kw(Keyword::Then) => todo!(),
                    TokenKind::Kw(Keyword::Else) => todo!(),
                    TokenKind::Kw(Keyword::Function) => todo!(),
                    TokenKind::Kw(Keyword::Match) => todo!(),
                    TokenKind::Kw(Keyword::With) => todo!(),
                    TokenKind::Kw(Keyword::Forall) => todo!(),
                    TokenKind::Kw(Keyword::Exists) => todo!(),
                    TokenKind::Ident(ident) => Some(self.make_expr(Expr::Var(Var(ident)))),
                    TokenKind::Numeral(n) => Some(self.make_expr(Expr::Num(Num(n)))),
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
    use super::*;

    #[test]
    fn parse_nothing() {
        let result = parse("");
        assert_eq!(result.decls().next(), None);
    }

    #[test]
    fn parse_module_decl() {
        let result = parse("module A.B.C");

        let module = result.get_decl(result.get_root()).get_module().unwrap();
        let path = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        assert_eq!(module.path, Some(path))
    }

    #[test]
    fn parse_import_decl() {
        let result = parse("import A.B.C");
        let mut iter = result.decls();

        let import_decl_id = iter.next().unwrap();
        let import_decl = result.get_decl(*import_decl_id).get_import().unwrap();
        assert_eq!(import_decl.path, vec!["A", "B", "C"]);

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn parse_let_five_be_5() {
        let result = parse("let five = 5");
        let mut iter = result.decls();

        let let_decl_id = iter.next().unwrap();
    }

    #[test]
    fn parse_let_paren_x_paren_be_2() {
        let result = parse("let (x) = 2");
        let mut iter = result.decls();

        let let_decl_id = iter.next().unwrap();
        let let_decl = result.get_decl(*let_decl_id).get_let().unwrap();

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn parse_let_id_which_is_a_to_a() {
        let result = parse("let id : a -> a");
        let mut iter = result.decls();

        let let_decl_id = iter.next().unwrap();
        let let_decl = result.get_decl(*let_decl_id).get_let().unwrap();

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn parse_let_id_x_be_x() {
        let result = parse("let id x = x");
        let mut iter = result.decls();

        let let_decl_id = iter.next().unwrap();
        let let_decl = result.get_decl(*let_decl_id).get_let().unwrap();

        assert_eq!(iter.next(), None);
    }
}
