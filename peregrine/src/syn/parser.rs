use crate::ast::decl::{Decl, DeclId, DeclKind};
use crate::ast::expr::{Expr, ExprId, ExprKind, Stmt};
use crate::ast::node::NodeId;
use crate::ast::{Ast, TokenSpan};

use crate::syn::cursor::{Delimiter, Gate};
use crate::syn::lexer::{ByteString, Keyword, Lexer, Token, TokenId, TokenKind};
use crate::syn::{ParseResult, SourceModule, SyntaxError};

use crate::idx;

pub(crate) struct Parser {
    lexer: Lexer,
    lookahead: Option<TokenId>,
    errors: Vec<(NodeId, SyntaxError)>,
    exprs: idx::Generation<ExprId>,
    decls: idx::Generation<DeclId>,
}

enum Precedence {
    TyAnn, // e : T
    App,   // f e
    Assoc, // (e)
}

impl Parser {
    fn new(input: impl Into<String>) -> Parser {
        Parser {
            lexer: Lexer::new(input),
            lookahead: None,
            errors: Vec::new(),
            exprs: idx::Generation::new(),
            decls: idx::Generation::new(),
        }
    }

    pub(crate) fn parse(input: impl Into<String>) -> ParseResult {
        let mut parser = Parser::new(input);
        let decls = parser.parse_decls();

        // Sanity: if we're done parsing, we should have no more tokens.
        assert!(parser.next().is_none());

        ParseResult {
            source_module: SourceModule::new(Ast::new(decls), parser.lexer.into()),
            errors: parser.errors,
        }
    }

    fn next(&mut self) -> Option<TokenId> {
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

    fn try_consume<F>(&mut self, f: F) -> Option<TokenId>
    where
        F: FnOnce(&Token) -> bool,
    {
        if self.lookahead.is_none() {
            self.lookahead = self.next();
        }

        let Some(tok) = self.lookahead else {
            return None;
        };

        if f(&self.lexer[tok]) {
            self.lookahead.take()
        } else {
            None
        }
    }

    fn try_parse_kw(&mut self, kw: Keyword) -> Option<TokenId> {
        self.try_consume(|tok| match tok.kind() {
            TokenKind::Kw(kw2) => &kw == kw2,
            _ => false,
        })
    }

    fn try_parse_operator(&mut self, str: impl AsRef<str>) -> Option<TokenId> {
        self.try_consume(|tok| match tok.kind() {
            TokenKind::Operator(op) => op == str.as_ref(),
            _ => false,
        })
    }

    fn try_parse_delimiter(&mut self, delim: Delimiter) -> Option<TokenId> {
        self.try_consume(|tok| match tok.kind() {
            TokenKind::Delimiter(d) => d == &delim,
            _ => false,
        })
    }

    fn make_expr_error(
        &mut self,
        err: SyntaxError,
        id: ExprId,
        begin_tok: TokenId,
        end_tok: TokenId,
    ) -> Expr {
        let expr = self.make_expr(ExprKind::Error(Some(id)), begin_tok, end_tok);
        self.errors.push((expr.id().into(), err));
        expr
    }

    fn report_expr_error(
        &mut self,
        err: SyntaxError,
        begin_tok: TokenId,
        end_tok: TokenId,
    ) -> Expr {
        let expr = self.make_expr(ExprKind::Error(None), begin_tok, end_tok);
        self.errors.push((expr.id().into(), err));
        expr
    }

    fn make_decl_error(
        &mut self,
        err: SyntaxError,
        id: DeclId,
        begin_tok: TokenId,
        end_tok: TokenId,
    ) -> Decl {
        let decl = self.make_decl(DeclKind::Error(Some(id)), begin_tok, end_tok);
        self.errors.push((decl.id().into(), err));
        decl
    }

    fn report_decl_error(
        &mut self,
        err: SyntaxError,
        begin_tok: TokenId,
        end_tok: TokenId,
    ) -> Decl {
        let decl = self.make_decl(DeclKind::Error(None), begin_tok, end_tok);
        self.errors.push((decl.id().into(), err));
        decl
    }

    fn make_expr(&mut self, expr: ExprKind, begin_tok: TokenId, end_tok: TokenId) -> Expr {
        Expr::new(self.exprs.next(), expr, TokenSpan::new(begin_tok, end_tok))
    }

    fn make_decl(&mut self, decl: DeclKind, begin_tok: TokenId, end_tok: TokenId) -> Decl {
        Decl::new(self.decls.next(), decl, TokenSpan::new(begin_tok, end_tok))
    }

    fn parse_decl(&mut self) -> Option<Decl> {
        let tok = self.next()?;
        self.parse_decl_tail(tok)
    }

    fn parse_decl_tail(&mut self, tok: TokenId) -> Option<Decl> {
        match self.lexer[tok].kind() {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(Keyword::Module) => Some(self.parse_module_tail(tok)),
            TokenKind::Kw(Keyword::Import) => Some(self.parse_import_tail(tok)),
            TokenKind::Kw(Keyword::Export) => Some(self.parse_export_tail(tok)),
            TokenKind::Kw(Keyword::Public) => Some(self.parse_public_tail(tok)),
            TokenKind::Kw(Keyword::Open) => Some(self.parse_open_tail(tok)),
            TokenKind::Kw(Keyword::Hiding) => todo!(),
            TokenKind::Kw(Keyword::Renaming) => todo!(),
            TokenKind::Kw(Keyword::Struct) => Some(self.parse_struct_tail(tok)),
            TokenKind::Kw(Keyword::Data) => Some(self.parse_data_tail(tok)),
            TokenKind::Kw(Keyword::Deriving) => todo!(),
            TokenKind::Kw(Keyword::Class) => Some(self.parse_class_tail(tok)),
            TokenKind::Kw(Keyword::Instance) => Some(self.parse_instance_tail(tok)),
            TokenKind::Kw(Keyword::Where) => todo!(), // TODO: needs serious error recovery
            TokenKind::Kw(Keyword::Let) => Some(self.parse_let_decl_tail(tok)), // TODO: stop supporting this as Decl
            TokenKind::Kw(Keyword::In) => todo!(), // TODO: needs serious error recovery
            TokenKind::Kw(Keyword::Do) => None,
            TokenKind::Kw(Keyword::If) => None,
            TokenKind::Kw(Keyword::Then) => None,
            TokenKind::Kw(Keyword::Else) => None,
            TokenKind::Kw(Keyword::Function) => None,
            TokenKind::Kw(Keyword::Match) => None,
            TokenKind::Kw(Keyword::With) => None,
            TokenKind::Kw(Keyword::Forall) => None,
            TokenKind::Kw(Keyword::Exists) => None,
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => None,
            TokenKind::ByteString(_) => None,
            TokenKind::Operator(_) => None,
            TokenKind::Delimiter(Delimiter::Paren(Gate::Opened)) => todo!(),
            TokenKind::Delimiter(_) => todo!(), // TODO: no other delimiters are valid here.
        }
    }

    fn parse_decls(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();

        while let Some(decl) = self.parse_decl() {
            decls.push(decl);
        }

        decls
    }

    fn parse_path(&mut self) -> (bool, Vec<String>, Option<TokenId>) {
        let mut ok = true;
        let mut path = Vec::new();
        let mut last_tok = None;

        loop {
            let Some(id) = self.next() else {
                ok = false;
                break;
            };

            last_tok = Some(id);

            if let TokenKind::Ident(ident) = &self.lexer[id].kind() {
                path.push(ident.clone());

                let dot_tok = self.try_parse_operator(".");
                if dot_tok.is_none() {
                    break;
                } else {
                    last_tok = dot_tok;
                }
            } else {
                ok = false;
            }
        }

        (ok, path, last_tok)
    }

    fn parse_module_tail(&mut self, module_tok: TokenId) -> Decl {
        let (ok, path, last_tok) = self.parse_path();

        // TODO: `where` keyword here? e.g. we _might_ have two kinds of `module` declarations.
        // But I'm halfminded here, the filesystem already implies `module List`. Not sure yet.
        //
        // I'm leaving this as is for now. I need to finish the design of the module system and
        // the build system in order to make sense of it all.
        //
        // #1: top-level `module` declaration:
        //
        //  module List
        //
        //  export data List a =
        //    | Nil
        //    | Cons a (List a)
        //
        //  let length : List a -> Natural
        //  let length =
        //    function
        //    | Nil -> 0
        //    | Cons _ xs -> 1 + length xs
        //
        // #2: non-toplevel `module` declaration:
        //
        //  module List where
        //    export data List a
        //      | Nil
        //      | Cons a (List a)
        //
        //    let length : List a -> Natural
        //    let length =
        //      function
        //      | Nil -> 0
        //      | Cons _ xs -> 1 + length xs
        let decls = self.parse_decls();

        let decl = self.make_decl(
            DeclKind::module(Some(path), decls),
            module_tok,
            last_tok.unwrap_or(module_tok),
        );

        if ok {
            decl
        } else {
            self.make_decl_error(
                SyntaxError::MissingName,
                decl.id(),
                module_tok,
                last_tok.unwrap_or(module_tok),
            )
        }
    }

    fn parse_import_tail(&mut self, import_tok: TokenId) -> Decl {
        let (ok, path, last_tok) = self.parse_path();

        let decl = self.make_decl(
            DeclKind::import(path),
            import_tok,
            last_tok.unwrap_or(import_tok),
        );

        if ok {
            decl
        } else {
            self.make_decl_error(
                SyntaxError::MissingName,
                decl.id(),
                import_tok,
                last_tok.unwrap_or(import_tok),
            )
        }
    }

    fn parse_export_tail(&mut self, export_tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            let last_tok = inner.end_token();
            return self.make_decl(DeclKind::export(inner), export_tok, last_tok);
        }

        self.report_decl_error(SyntaxError::MissingDecl, export_tok, export_tok)
    }

    fn parse_public_tail(&mut self, public_tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            let last_tok = inner.end_token();
            return self.make_decl(DeclKind::public(inner), public_tok, last_tok);
        }

        self.report_decl_error(SyntaxError::MissingDecl, public_tok, public_tok)
    }

    fn parse_open_tail(&mut self, open_tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            let last_tok = inner.end_token();
            return self.make_decl(DeclKind::open(inner), open_tok, last_tok);
        }

        self.report_decl_error(SyntaxError::MissingDecl, open_tok, open_tok)
    }

    fn parse_struct_tail(&mut self, struct_tok: TokenId) -> Decl {
        let sig = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, struct_tok, struct_tok)
        });

        if self.try_parse_kw(Keyword::Where).is_none() {
            let last_tok = sig.end_token();
            return self.make_decl(DeclKind::structure(sig, Vec::new()), struct_tok, last_tok);
        }

        todo!("layout rules");
    }

    fn parse_data_tail(&mut self, data_tok: TokenId) -> Decl {
        let sig = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, data_tok, data_tok)
        });

        if let Some(where_tok) = self.try_parse_kw(Keyword::Where) {
            todo!("layout rules");
        }

        if let Some(eq_tok) = self.try_parse_operator("=") {
            todo!("layout rules");
        }

        let last_tok = sig.end_token();
        self.make_decl(DeclKind::empty(sig), data_tok, last_tok)
    }

    fn parse_class_tail(&mut self, class_tok: TokenId) -> Decl {
        let sig = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, class_tok, class_tok)
        });

        if let Some(where_tok) = self.try_parse_kw(Keyword::Where) {
            todo!("layout rules");
        }

        let last_tok = sig.end_token();
        self.make_decl(DeclKind::class(sig, Vec::new()), class_tok, last_tok)
    }

    fn parse_instance_tail(&mut self, instance_tok: TokenId) -> Decl {
        let sig = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, instance_tok, instance_tok)
        });

        if let Some(where_tok) = self.try_parse_kw(Keyword::Where) {
            todo!("layout rules");
        }

        let last_tok = sig.end_token();
        self.make_decl(DeclKind::instance(sig, Vec::new()), instance_tok, last_tok)
    }

    fn parse_let_decl_tail(&mut self, let_tok: TokenId) -> Decl {
        let f = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr, let_tok, let_tok));

        // TODO: if eq_tok is None and the layout rule did not dedent, then
        // perhaps the user forgot to supply an `= expr`.
        let Some(eq_tok) = self.try_parse_operator("=") else {
            let last_tok = f.end_token();
            return self.make_decl(DeclKind::let_the_expr(f), let_tok, last_tok);
        };

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(DeclKind::let_the_expr(f), let_tok, eq_tok);
            return self.make_decl_error(SyntaxError::MissingExpr, decl.id(), let_tok, eq_tok);
        };

        let last_tok = e.end_token();
        let decl = self.make_decl(DeclKind::let_the_expr_be(f, e), let_tok, last_tok);

        // We're parsing `in` here just for error reporting.
        let Some(in_tok) = self.try_parse_kw(Keyword::In) else {
            return decl;
        };

        let i = self.parse_expr(Precedence::TyAnn);
        self.make_decl_error(
            SyntaxError::LetDeclCannotHaveIn(i.as_ref().map(|e| e.id())),
            decl.id(),
            let_tok,
            i.map(|e| e.end_token()).unwrap_or(in_tok),
        )
    }

    // TODO: Stop using Option<Expr> here and rely on layout rules
    fn parse_expr(&mut self, pred: Precedence) -> Option<Expr> {
        match pred {
            Precedence::TyAnn => self.parse_ty_expr(),
            Precedence::App => self.parse_app_expr(),
            Precedence::Assoc => self.parse_assoc_expr(),
        }
    }

    fn parse_ty_expr(&mut self) -> Option<Expr> {
        let expr = self.parse_expr(Precedence::App)?;

        Some(self.parse_ty_expr_tail(expr))
    }

    fn parse_ty_expr_tail(&mut self, expr: Expr) -> Expr {
        let Some(colon_tok) = self.try_parse_operator(":") else {
            return expr;
        };

        // Beautiful, beautiful Curry-Howard correspondence.
        // No need for a whole separate language for types. :)
        let ty = self.parse_expr(Precedence::App);

        let first_tok = expr.begin_token();

        match ty {
            Some(ty) => {
                let last_tok = ty.end_token();
                self.make_expr(ExprKind::ann(expr, ty), first_tok, last_tok)
            }
            None => {
                let ty = self.report_expr_error(SyntaxError::MissingExpr, colon_tok, colon_tok);
                let last_tok = ty.end_token();
                self.make_expr(ExprKind::ann(expr, ty), first_tok, last_tok)
            }
        }
    }

    fn parse_app_expr(&mut self) -> Option<Expr> {
        let tok = self.next()?;

        match self.lexer[tok].kind() {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(Keyword::Module) => None,
            TokenKind::Kw(Keyword::Import) => todo!(), // TODO: support, scoped imports are nice.
            TokenKind::Kw(Keyword::Export) => None,
            TokenKind::Kw(Keyword::Public) => None,
            TokenKind::Kw(Keyword::Open) => todo!(), // TODO: support, scoped opens are nice.
            TokenKind::Kw(Keyword::Hiding) => None,
            TokenKind::Kw(Keyword::Renaming) => todo!(), // TODO: support, scoped renaming is nice.
            TokenKind::Kw(Keyword::Struct) => None,
            TokenKind::Kw(Keyword::Data) => None,
            TokenKind::Kw(Keyword::Deriving) => None,
            TokenKind::Kw(Keyword::Class) => None,
            TokenKind::Kw(Keyword::Instance) => None,
            TokenKind::Kw(Keyword::Where) => None,
            TokenKind::Kw(Keyword::Let) => Some(self.parse_let_expr_tail(tok)),
            TokenKind::Kw(Keyword::In) => None,
            TokenKind::Kw(Keyword::Do) => Some(self.parse_do_expr_tail(tok)),
            TokenKind::Kw(Keyword::If) => Some(self.parse_if_expr_tail(tok)),
            TokenKind::Kw(Keyword::Then) => None,
            TokenKind::Kw(Keyword::Else) => None,
            TokenKind::Kw(Keyword::Function) => todo!(), // TODO: support
            TokenKind::Kw(Keyword::Match) => todo!(),    // TODO: support
            TokenKind::Kw(Keyword::With) => None,
            TokenKind::Kw(Keyword::Forall) => todo!(), // TODO: support
            TokenKind::Kw(Keyword::Exists) => todo!(), // TODO: support
            TokenKind::Ident(ident) => Some(self.parse_ident_tail(tok, ident.clone())),
            TokenKind::Numeral(num) => Some(self.parse_numeral_tail(tok, num.clone())),
            TokenKind::ByteString(s) => Some(self.parse_string_tail(tok, s.clone())),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(Delimiter::Paren(Gate::Opened)) => {
                Some(self.parse_assoc_expr_tail(tok))
            }
            TokenKind::Delimiter(Delimiter::Brace(Gate::Opened)) => todo!(),
            TokenKind::Delimiter(Delimiter::Bracket(Gate::Opened)) => todo!(),
            TokenKind::Delimiter(Delimiter::Paren(Gate::Closed)) => None,
            TokenKind::Delimiter(Delimiter::Brace(Gate::Closed)) => None,
            TokenKind::Delimiter(Delimiter::Bracket(Gate::Closed)) => None,
            TokenKind::Delimiter(Delimiter::Semicolon) => todo!(), // TODO: error,
            TokenKind::Delimiter(Delimiter::Comma) => todo!(),     // TODO: error,
        }
    }

    fn parse_let_expr_tail(&mut self, let_tok: TokenId) -> Expr {
        let Some(f) = self.parse_expr(Precedence::TyAnn) else {
            return self.report_expr_error(SyntaxError::MissingExpr, let_tok, let_tok);
        };

        let Some(eq_tok) = self.try_parse_operator("=") else {
            let first_tok = f.begin_token();
            let last_tok = f.end_token();

            // We want the error span to be at:
            //
            //  let f x
            //      ~~~
            let e = self.report_expr_error(
                SyntaxError::LetExprIsRequiredToHaveEquations,
                first_tok,
                last_tok,
            );

            return self.make_expr(ExprKind::let_be(f, e), let_tok, last_tok);
        };

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let e = self.report_expr_error(SyntaxError::MissingExpr, eq_tok, eq_tok);
            return self.make_expr(ExprKind::let_be(f, e), let_tok, eq_tok);
        };

        let Some(in_tok) = self.try_parse_kw(Keyword::In) else {
            let last_tok = e.end_token();
            return self.make_expr(ExprKind::let_be(f, e), let_tok, last_tok);
        };

        let Some(i) = self.parse_expr(Precedence::TyAnn) else {
            let i = self.report_expr_error(SyntaxError::MissingExpr, in_tok, in_tok);
            return self.make_expr(ExprKind::let_be_in(f, e, i), let_tok, in_tok);
        };

        let last_tok = i.end_token();
        self.make_expr(ExprKind::let_be_in(f, e, i), let_tok, last_tok)
    }

    fn parse_do_expr_tail(&mut self, do_tok: TokenId) -> Expr {
        let mut stmts = Vec::new();

        while let Some(expr) = self.parse_expr(Precedence::TyAnn) {
            stmts.push(Stmt::new(expr));
        }

        let last_tok = stmts.last().map(|s| s.0.end_token()).unwrap_or(do_tok);
        self.make_expr(ExprKind::do_notation(stmts), do_tok, last_tok)
    }

    fn parse_if_expr_tail(&mut self, if_tok: TokenId) -> Expr {
        let antecedent = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr, if_tok, if_tok));
        let then_tok = self.try_parse_kw(Keyword::Then);
        let last_tok = then_tok.unwrap_or(if_tok);
        let consequent = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, last_tok, last_tok)
        });
        let else_tok = self.try_parse_kw(Keyword::Else);
        let last_tok = else_tok.unwrap_or(last_tok);
        let alternative = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, last_tok, last_tok)
        });

        let last_tok = alternative.end_token();

        if then_tok.is_some() && else_tok.is_some() {
            return self.make_expr(
                ExprKind::if_then_else(antecedent, consequent, alternative),
                if_tok,
                last_tok,
            );
        }

        let expr = self.make_expr(
            ExprKind::if_then_else(antecedent, consequent, alternative),
            if_tok,
            last_tok,
        );

        let err = SyntaxError::IfExpr {
            missing_if_kw: false,
            missing_then_kw: then_tok.is_none(),
            missing_else_kw: else_tok.is_none(),
        };

        self.make_expr_error(err, expr.id(), if_tok, last_tok)
    }

    fn parse_ident_tail(&mut self, ident_tok: TokenId, ident: String) -> Expr {
        self.make_expr(ExprKind::var(ident), ident_tok, ident_tok)
    }

    fn parse_numeral_tail(&mut self, num_tok: TokenId, num: String) -> Expr {
        self.make_expr(ExprKind::num(num), num_tok, num_tok)
    }

    fn parse_string_tail(&mut self, str_tok: TokenId, str: ByteString) -> Expr {
        self.make_expr(ExprKind::str(str), str_tok, str_tok)
    }

    fn parse_assoc_expr_tail(&mut self, opened_tok: TokenId) -> Expr {
        // Two missed opportunities to report errors.
        let e = self.parse_expr(Precedence::Assoc);
        let Some(closed_tok) = self.try_parse_delimiter(Delimiter::Paren(Gate::Closed)) else {
            let err = SyntaxError::NotBalanced(Delimiter::Paren(Gate::Closed));
            return match e {
                Some(e) => self.make_expr_error(err, e.id(), opened_tok, e.end_token()),
                None => self.report_expr_error(
                    SyntaxError::NotBalanced(Delimiter::Paren(Gate::Closed)),
                    opened_tok,
                    opened_tok,
                ),
            };
        };

        match e {
            Some(e) => self.make_expr(ExprKind::assoc(e), opened_tok, closed_tok),
            None => self.make_expr(ExprKind::unit(), opened_tok, closed_tok),
        }
    }

    fn parse_assoc_expr(&mut self) -> Option<Expr> {
        let tok = self.next()?;

        match self.lexer[tok].kind() {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(_) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::ByteString(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{decl, expr, Position};
    use crate::syn;

    #[test]
    fn parse_nothing() {
        let result = syn::parse("");
        assert!(result.ast().decls().is_empty());
    }

    #[test]
    fn parse_module_decl() {
        let result = syn::parse("module A.B.C");
        let mut iter = result.ast().decls().iter();

        let module_decl = iter.next().unwrap();
        let module = module_decl.as_module().unwrap();
        let path = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        assert_eq!(module.path(), Some(&path));

        assert!(iter.next().is_none());
    }

    #[test]
    fn parse_import_decl() {
        let result = syn::parse("import A.B.C");
        let source_module = &result.source_module;
        let mut iter = result.ast().decls().iter();

        let import_decl = iter.next().unwrap();
        let import = import_decl.as_import().unwrap();
        assert_eq!(import.path(), &vec!["A", "B", "C"]);

        let import_src_span = source_module.source_span(import_decl);
        assert_eq!(import_src_span.begin, Position::new(1, 0));
        assert_eq!(import_src_span.end, Position::new(1, 12));

        assert!(iter.next().is_none());
    }

    #[test]
    fn parse_export_decl() {
        let source_module = syn::parse("export let five = 5").source_module;
        let mut iter = source_module.ast.decls().iter();

        let export_decl = iter.next().unwrap();
        let export = export_decl.as_export().unwrap();
        let let_decl = export.decl().as_let().unwrap();

        match let_decl {
            decl::Let::Decl(..) => panic!("not this one"),
            decl::Let::DeclExpr(f, e) => {
                let expr::Var(var) = f.as_var().unwrap();
                assert_eq!(var, "five");

                let expr::Num(num) = e.as_num().unwrap();
                assert_eq!(num, "5");
            }
        }
    }

    #[test]
    fn parse_let_five_be_5() {
        let source_module = syn::parse("let five = 5").source_module;
        let mut iter = source_module.ast.decls().iter();

        let let_decl = iter.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::Decl(..) => panic!("not this one"),
            decl::Let::DeclExpr(f, e) => {
                let expr::Var(var) = f.as_var().unwrap();
                assert_eq!(var, "five");

                let expr::Num(num) = e.as_num().unwrap();
                assert_eq!(num, "5");
            }
        }
    }

    #[test]
    fn parse_let_paren_x_paren_be_2() {
        let source_module = syn::parse("let (x) = 2").source_module;
        let mut iter = source_module.ast.decls().iter();

        let let_decl = iter.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::Decl(..) => panic!("not this one"),
            decl::Let::DeclExpr(f, e) => {
                let expr::Var(var) = f.as_var().unwrap();
                assert_eq!(var, "x");

                let expr::Num(num) = e.as_num().unwrap();
                assert_eq!(num, "2");
            }
        }
    }

    #[test]
    fn parse_let_id_which_is_a_to_a() {
        let source_module = syn::parse("let id : a -> a").source_module;
        let mut iter = source_module.ast.decls().iter();

        let let_decl = iter.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::DeclExpr(..) => panic!("not this one"),
            decl::Let::Decl(f) => {
                let expr::Ann(e, t) = f.as_ann().unwrap();
                let expr::Var(var) = e.as_var().unwrap();
                assert_eq!(var, "id");

                let app_2 = t.as_app().unwrap();
                let expr::Var(arg_a_2) = app_2.argument().as_var().unwrap();
                assert_eq!(arg_a_2, "a");

                let app_1 = app_2.function().as_app().unwrap();
                let expr::Var(arg_a_1) = app_1.argument().as_var().unwrap();
                assert_eq!(arg_a_1, "a");

                let expr::Var(var_arrow) = app_1.function().as_var().unwrap();
                assert_eq!(var_arrow, "->");
            }
        }
    }

    #[test]
    fn parse_let_id_x_be_x() {
        let source_module = syn::parse("let id x = x").source_module;
        let mut iter = source_module.ast.decls().iter();

        let let_decl = iter.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::Decl(..) => panic!("not this one"),
            decl::Let::DeclExpr(f_id, e_id) => {
                let expr::App(f, e) = f_id.as_app().unwrap();
                let expr::Var(id) = f.as_var().unwrap();
                let expr::Var(x) = e_id.as_var().unwrap();
                assert_eq!(id, "id");
                assert_eq!(x, "x");

                let expr::Var(x) = e.as_var().unwrap();
                assert_eq!(x, "2");
            }
        }
    }

    #[test]
    fn parse_let_five_of_missing_type() {
        let result = syn::parse("let five :");
        let source_module = result.source_module;
        let errors = result.errors;

        let mut decls = source_module.ast.decls().iter();

        let let_decl = decls.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::DeclExpr(..) => panic!("not this one"),
            decl::Let::Decl(f) => {
                let expr::Ann(e, t) = f.as_ann().unwrap();
                let expr::Var(five) = e.as_var().unwrap();
                assert_eq!(five, "five");
                assert_eq!(t.as_error().unwrap(), None);

                let error = errors
                    .iter()
                    .find_map(|(id, e)| (id == &t.id()).then_some(e))
                    .unwrap();

                assert_eq!(error, &syn::SyntaxError::MissingExpr);
            }
        }
    }

    #[test]
    fn parse_unit() {
        let source_module = syn::parse("let unit = ()").source_module;
        let mut decls = source_module.ast.decls().iter();

        let let_decl = decls.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::Decl(_) => panic!("not this one"),
            decl::Let::DeclExpr(_, e) => assert!(e.is_unit()),
        }
    }

    // #[test]
    // fn parsing_preserves_spans() {
    //     let result = parse("let five = 5");
    //     let mut decls = result.decls();

    //     let let_decl_id = decls.next().unwrap();
    //     let let_decl_span = result.get_decl_span(*let_decl_id);

    //     assert_eq!(let_decl_span.begin, Position::new(1, 0));
    //     assert_eq!(let_decl_span.end, Position::new(1, 12));
    // }
}
