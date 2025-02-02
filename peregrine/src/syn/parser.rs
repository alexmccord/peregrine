use crate::ast::decl::{Decl, DeclId, DeclKind};
use crate::ast::expr::{Expr, ExprId, ExprKind, Stmt};
use crate::ast::node::NodeId;
use crate::ast::Ast;

use crate::syn::cursor::{Delimiter, Gate};
use crate::syn::lexer::{Keyword, Lexer, Token, TokenId, TokenKind};
use crate::syn::{ParseResult, SyntaxError};

use crate::idx;

pub(crate) struct Parser {
    lexer: Lexer,
    lookahead: Option<TokenId>,
    errors: Vec<(NodeId, SyntaxError)>,
    tokens: idx::IndexedVec<Token>,
    exprs: idx::Generation<Expr>,
    decls: idx::Generation<Decl>,
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
            tokens: idx::IndexedVec::new(),
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
            ast: Ast::new(decls),
            errors: parser.errors,
            tokens: parser.tokens,
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

        let tok = self.lexer.next_token()?;
        let id = tok.id();
        self.tokens.insert(id, tok);
        Some(id)
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

        if f(&self.tokens[tok]) {
            self.lookahead.take()
        } else {
            None
        }
    }

    fn try_parse_kw(&mut self, kw: Keyword) -> Option<TokenId> {
        self.try_consume(|tok| tok.is_kw(kw))
    }

    fn try_parse_operator(&mut self, str: impl AsRef<str>) -> Option<TokenId> {
        self.try_consume(|tok| tok.is_operator(str))
    }

    fn try_parse_delimiter(&mut self, delim: Delimiter) -> Option<TokenId> {
        self.try_consume(|tok| match tok.kind() {
            TokenKind::Delimiter(d) => d == &delim,
            _ => false,
        })
    }

    fn make_err_expr(&mut self, err: SyntaxError, id: ExprId) -> Expr {
        let expr = self.make_expr(ExprKind::Error(Some(id)));
        self.errors.push((expr.id().into(), err));
        expr
    }

    fn report_expr_error(&mut self, err: SyntaxError) -> Expr {
        let expr = self.make_expr(ExprKind::Error(None));
        self.errors.push((expr.id().into(), err));
        expr
    }

    fn make_err_decl(&mut self, err: SyntaxError, id: DeclId) -> Decl {
        let decl = self.make_decl(DeclKind::Error(Some(id)));
        self.errors.push((decl.id().into(), err));
        decl
    }

    fn report_decl_error(&mut self, err: SyntaxError) -> Decl {
        let decl = self.make_decl(DeclKind::Error(None));
        self.errors.push((decl.id().into(), err));
        decl
    }

    fn make_expr(&mut self, expr: ExprKind) -> Expr {
        Expr::new(self.exprs.next(), expr)
    }

    fn make_decl(&mut self, decl: DeclKind) -> Decl {
        Decl::new(self.decls.next(), decl)
    }

    fn parse_decl(&mut self) -> Option<Decl> {
        let tok = self.next()?;
        self.parse_decl_tail(tok)
    }

    fn parse_decl_tail(&mut self, tok: TokenId) -> Option<Decl> {
        match self.tokens[tok].kind() {
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

    fn parse_path(&mut self) -> (bool, Vec<String>) {
        let mut ok = true;
        let mut path = Vec::new();

        loop {
            let tok = self.next();
            if let Some(ident) = tok.map(|id| &self.tokens[id]).and_then(Token::get_ident) {
                path.push(ident.clone());

                if self.try_parse_operator(".").is_none() {
                    break;
                }
            } else {
                ok = false;
            }
        }

        (ok, path)
    }

    fn parse_module_tail(&mut self, tok: TokenId) -> Decl {
        let (ok, path) = self.parse_path();

        let decls = self.parse_decls();

        let decl = self.make_decl(DeclKind::module(Some(path), decls));

        if ok {
            decl
        } else {
            self.make_err_decl(SyntaxError::MissingName, decl.id())
        }
    }

    fn parse_import_tail(&mut self, tok: TokenId) -> Decl {
        let (ok, path) = self.parse_path();

        let decl = self.make_decl(DeclKind::import(path));

        if ok {
            decl
        } else {
            self.make_err_decl(SyntaxError::MissingName, decl.id())
        }
    }

    fn parse_export_tail(&mut self, tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            return self.make_decl(DeclKind::export(inner));
        }

        self.report_decl_error(SyntaxError::MissingDecl)
    }

    fn parse_public_tail(&mut self, tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            return self.make_decl(DeclKind::public(inner));
        }

        self.report_decl_error(SyntaxError::MissingDecl)
    }

    fn parse_open_tail(&mut self, tok: TokenId) -> Decl {
        if let Some(inner) = self.parse_decl() {
            return self.make_decl(DeclKind::open(inner));
        }

        self.report_decl_error(SyntaxError::MissingDecl)
    }

    fn parse_struct_tail(&mut self, tok: TokenId) -> Decl {
        let sig = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        if self.try_parse_kw(Keyword::Where).is_none() {
            return self.make_decl(DeclKind::structure(sig, Vec::new()));
        }

        todo!("layout rules");
    }

    fn parse_data_tail(&mut self, tok: TokenId) -> Decl {
        let sig = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        if self.try_parse_kw(Keyword::Where).is_some() {
            todo!("layout rules");
        }

        if self.try_parse_operator("=").is_some() {
            todo!("layout rules");
        }

        self.make_decl(DeclKind::empty(sig))
    }

    fn parse_class_tail(&mut self, tok: TokenId) -> Decl {
        let sig = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        if self.try_parse_kw(Keyword::Where).is_some() {
            todo!("layout rules");
        }

        self.make_decl(DeclKind::class(sig, Vec::new()))
    }

    fn parse_instance_tail(&mut self, tok: TokenId) -> Decl {
        let sig = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        if self.try_parse_kw(Keyword::Where).is_some() {
            todo!("layout rules");
        }

        self.make_decl(DeclKind::instance(sig, Vec::new()))
    }

    fn parse_let_decl_tail(&mut self, tok: TokenId) -> Decl {
        let f = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        // TODO: if eq_tok is None and the layout rule did not dedent, then
        // perhaps the user forgot to supply an `= expr`.
        let Some(eq_tok) = self.try_parse_operator("=") else {
            return self.make_decl(DeclKind::let_the_expr(f));
        };

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let decl = self.make_decl(DeclKind::let_the_expr(f));
            return self.make_err_decl(SyntaxError::MissingExpr, decl.id());
        };

        let decl = self.make_decl(DeclKind::let_the_expr_be(f, e));

        if self.try_parse_kw(Keyword::In).is_none() {
            return decl;
        };

        let i = self.parse_expr(Precedence::TyAnn);
        self.make_err_decl(
            SyntaxError::LetDeclCannotHaveIn(i.map(|e| e.id())),
            decl.id(),
        )
    }

    // TODO: Stop using Option<Expr> here and rely on layout rules
    fn parse_expr(&mut self, pred: Precedence) -> Option<Expr> {
        match pred {
            Precedence::TyAnn => Some(self.parse_ty_expr()),
            Precedence::App => self.parse_app_expr(),
            Precedence::Assoc => self.parse_assoc_expr(),
        }
    }

    fn parse_ty_expr(&mut self) -> Expr {
        let expr = self
            .parse_expr(Precedence::App)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        self.parse_ty_expr_tail(expr)
    }

    fn parse_ty_expr_tail(&mut self, expr: Expr) -> Expr {
        if self.try_parse_operator(":").is_none() {
            return expr;
        };

        // Beautiful, beautiful Curry-Howard correspondence.
        // No need for a whole separate language for types. :)
        let ty = self.parse_expr(Precedence::App);

        match ty {
            Some(ty) => self.make_expr(ExprKind::ann(expr, ty)),
            None => {
                let ty = self.report_expr_error(SyntaxError::MissingExpr);

                self.make_expr(ExprKind::ann(expr, ty))
            }
        }
    }

    fn parse_app_expr(&mut self) -> Option<Expr> {
        let tok = self.next()?;

        match self.tokens[tok].kind() {
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
            TokenKind::Ident(ident) => Some(self.make_expr(ExprKind::var(ident.clone()))),
            TokenKind::Numeral(n) => Some(self.make_expr(ExprKind::num(n.clone()))),
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
            ) => Some(self.report_expr_error(SyntaxError::NotBalanced(d.clone()))),
            TokenKind::Delimiter(Delimiter::Semicolon) => todo!(), // TODO: error,
            TokenKind::Delimiter(Delimiter::Comma) => todo!(),     // TODO: error,
        }
    }

    fn parse_assoc_expr(&mut self) -> Option<Expr> {
        let tok = self.next()?;

        match self.tokens[tok].kind() {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(_) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(_) => todo!(),
        }
    }

    fn parse_let_expr_tail(&mut self, tok: TokenId) -> Expr {
        let Some(f) = self.parse_expr(Precedence::TyAnn) else {
            return self.report_expr_error(SyntaxError::MissingExpr);
        };

        if self.try_parse_operator("=").is_none() {
            let e = self.report_expr_error(SyntaxError::LetExprIsRequiredToHaveEquations);
            return self.make_expr(ExprKind::let_be(f, e));
        }

        let Some(e) = self.parse_expr(Precedence::TyAnn) else {
            let e = self.report_expr_error(SyntaxError::MissingExpr);
            return self.make_expr(ExprKind::let_be(f, e));
        };

        if self.try_parse_kw(Keyword::In).is_none() {
            return self.make_expr(ExprKind::let_be(f, e));
        };

        let Some(i) = self.parse_expr(Precedence::TyAnn) else {
            let i = self.report_expr_error(SyntaxError::MissingExpr);
            return self.make_expr(ExprKind::let_be_in(f, e, i));
        };

        self.make_expr(ExprKind::let_be_in(f, e, i))
    }

    fn parse_do_expr_tail(&mut self, tok: TokenId) -> Expr {
        let mut stmts = Vec::new();

        while let Some(expr) = self.parse_expr(Precedence::TyAnn) {
            stmts.push(Stmt::new(expr));
        }

        self.make_expr(ExprKind::do_notation(stmts))
    }

    fn parse_if_expr_tail(&mut self, tok: TokenId) -> Expr {
        let antecedent = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));
        let then_tok = self.try_parse_kw(Keyword::Then);
        let consequent = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));
        let else_tok = self.try_parse_kw(Keyword::Else);
        let alternative = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr));

        if then_tok.is_some() && else_tok.is_some() {
            return self.make_expr(ExprKind::if_then_else(antecedent, consequent, alternative));
        }

        let expr = self.make_expr(ExprKind::if_then_else(antecedent, consequent, alternative));
        let err = SyntaxError::IfExpr {
            missing_if_kw: false,
            missing_then_kw: then_tok.is_none(),
            missing_else_kw: else_tok.is_none(),
        };

        self.make_err_expr(err, expr.id())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{decl, expr};
    use crate::syn;

    #[test]
    fn parse_nothing() {
        let result = syn::parse("");
        assert!(result.ast.decls().is_empty());
    }

    #[test]
    fn parse_module_decl() {
        let result = syn::parse("module A.B.C");
        let mut iter = result.ast.decls().iter();

        let module_decl = iter.next().unwrap();
        let module = module_decl.as_module().unwrap();
        let path = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        assert_eq!(module.path(), Some(&path));

        assert!(iter.next().is_none());
    }

    #[test]
    fn parse_import_decl() {
        let result = syn::parse("import A.B.C");
        let mut iter = result.ast.decls().iter();

        let import_decl = iter.next().unwrap();
        let import = import_decl.as_import().unwrap();
        assert_eq!(import.path(), &vec!["A", "B", "C"]);

        assert!(iter.next().is_none());
    }

    #[test]
    fn parse_export_decl() {
        let result = syn::parse("export let five = 5");
        let mut iter = result.ast.decls().iter();

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
        let result = syn::parse("let five = 5");
        let mut iter = result.ast.decls().iter();

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
        let result = syn::parse("let (x) = 2");
        let mut iter = result.ast.decls().iter();

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
        let result = syn::parse("let id : a -> a");
        let mut iter = result.ast.decls().iter();

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
        let result = syn::parse("let id x = x");
        let mut iter = result.ast.decls().iter();

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
        let mut decls = result.ast.decls().iter();

        let let_decl = decls.next().unwrap();
        let l = let_decl.as_let().unwrap();

        match l {
            decl::Let::DeclExpr(..) => panic!("not this one"),
            decl::Let::Decl(f) => {
                let expr::Ann(e, t) = f.as_ann().unwrap();
                let expr::Var(five) = e.as_var().unwrap();
                assert_eq!(five, "five");
                assert_eq!(t.as_error().unwrap(), None);

                let error = result
                    .errors
                    .iter()
                    .find_map(|(id, e)| (id == &t.id()).then_some(e))
                    .unwrap();

                assert_eq!(error, &syn::SyntaxError::MissingExpr);
            }
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
