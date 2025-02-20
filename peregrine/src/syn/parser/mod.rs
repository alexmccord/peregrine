use crate::idx;

use crate::ast::decl::{Decl, DeclId, DeclKind, Path, PathNode};
use crate::ast::expr::{Expr, ExprId, ExprKind, Stmt};
use crate::ast::node::NodeId;
use crate::ast::{Ast, TokenSpan};
use crate::syn::lexer::tok::*;
use crate::syn::lexer::Lexer;
use crate::syn::offside::Absolute;
use crate::syn::{ParseResult, SourceModule, SyntaxError};

use super::offside::{Indentation, PartialOffsideOrd};

mod tests;

pub(crate) struct Parser {
    lexer: Lexer,
    lookahead: Option<(Option<Absolute>, Token)>,
    errors: Vec<(NodeId, SyntaxError)>,
    exprs: idx::Generation<ExprId>,
    decls: idx::Generation<DeclId>,
    tokens: TokenVec,
}

enum Precedence {
    TyAnn, // e : T
    App,   // f e
    Assoc, // (e)
    Atom,  // all terminals
}

// Simple state wrt whether to skip whitespace when advancing the lexer.
//
// Most of the grammar doesn't care about whitespace, but some does, ignoring
// the obvious like layouts.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Sensitivity {
    Sensitive,
    Insensitive,
}

impl Parser {
    fn new(input: impl Into<String>) -> Parser {
        Parser {
            lexer: Lexer::new(input),
            lookahead: None,
            errors: Vec::new(),
            exprs: idx::Generation::new(),
            decls: idx::Generation::new(),
            tokens: TokenVec::new(),
        }
    }

    pub(crate) fn parse(input: impl Into<String>) -> ParseResult {
        let mut parser = Parser::new(input);
        let decls = parser.parse_decls();

        // Sanity: if we're done parsing, we should have no more tokens.
        assert!(parser.next_tok().is_none());

        ParseResult {
            source_module: SourceModule::new(Ast::new(decls), parser.tokens),
            errors: parser.errors,
        }
    }

    fn consume_until(&mut self, sensitivity: Sensitivity) -> Option<(Option<Absolute>, TokenId)> {
        while let Some((offside, tok)) = self.lexer.next() {
            match tok.kind() {
                TokenKind::Ws(_) if sensitivity == Sensitivity::Insensitive => continue,
                _ => match self.lookahead.replace((offside, tok)) {
                    Some((offside, tok)) => return Some((offside, self.tokens.push(tok))),
                    None => (),
                },
            }
        }

        let (offside, tok) = self.lookahead.take()?;
        Some((offside, self.tokens.push(tok)))
    }

    fn next_non_ws_tok(&mut self) -> Option<(Option<Absolute>, TokenId)> {
        self.consume_until(Sensitivity::Insensitive)
    }

    fn next_tok(&mut self) -> Option<(Option<Absolute>, TokenId)> {
        self.consume_until(Sensitivity::Sensitive)
    }

    fn try_consume<F>(&mut self, f: F) -> Option<(Option<Absolute>, TokenId)>
    where
        F: FnOnce(&TokenKind) -> bool,
    {
        if f(&self.lookahead.as_ref()?.1.kind()) {
            self.next_non_ws_tok()
        } else {
            None
        }
    }

    fn try_parse_kw(&mut self, kw: Keyword) -> Option<(Option<Absolute>, TokenId)> {
        self.try_consume(|tok| match tok {
            TokenKind::Kw(kw2) => &kw == kw2,
            _ => false,
        })
    }

    fn try_parse_operator(&mut self, str: impl AsRef<str>) -> Option<(Option<Absolute>, TokenId)> {
        self.try_consume(|tok| match tok {
            TokenKind::Operator(op) => op == str.as_ref(),
            _ => false,
        })
    }

    fn try_parse_group(&mut self, group: Group) -> Option<(Option<Absolute>, TokenId)> {
        self.try_consume(|tok| match tok {
            TokenKind::Group(g) => g == &group,
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
        let (offside, tok) = self.next_non_ws_tok()?;
        self.parse_decl_tail(offside, tok)
    }

    fn parse_decl_tail(&mut self, offside: Option<Absolute>, tok: TokenId) -> Option<Decl> {
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
            TokenKind::ByteString(_) => None,
            TokenKind::Operator(_) => None,
            TokenKind::Group(_) => todo!(),
            TokenKind::Ws(_) => todo!(),
            TokenKind::Semicolon => todo!(),
            TokenKind::Comma => todo!(),
            TokenKind::Eof => None,
        }
    }

    fn parse_decls(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();

        while let Some(decl) = self.parse_decl() {
            decls.push(decl);
        }

        decls
    }

    fn parse_path(&mut self) -> (bool, Path, Option<TokenId>) {
        let mut ok = true;
        let mut path = Vec::new();
        let mut last_tok = None;

        loop {
            let Some((_, id)) = self.next_non_ws_tok() else {
                ok = false;
                break;
            };

            last_tok = Some(id);

            if let TokenKind::Ident(ident) = &self.tokens[id].kind() {
                path.push(PathNode::Name(ident.clone()));

                let Some((_, tok)) = self.try_parse_operator(".") else {
                    break;
                };

                last_tok = Some(tok);
            } else {
                ok = false;
                path.push(PathNode::Missing);
            }
        }

        (ok, Path::new(path), last_tok)
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
        let Some((_, eq_tok)) = self.try_parse_operator("=") else {
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
        let Some((_, in_tok)) = self.try_parse_kw(Keyword::In) else {
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

    fn parse_layout(&mut self, offside: Option<Absolute>) -> Option<Indentation> {
        loop {
            let (next_offside, tok) = self.next_tok()?;

            break match self.tokens[tok].kind() {
                TokenKind::Ws(_) => continue,
                TokenKind::Eof => None,
                _ => offside.partial_cmp_offside(&next_offside),
            };
        }
    }

    // When to use `self.parse_expr` or `self.parse_expr_by_tok`?
    //
    // That depends on the current parser state:
    //   1. Do you have a token that represents the current token? Use `parse_expr_by_tok`.
    //      This could be because you're doing something that needs to match on `tok` at
    //      multiple levels of recursion, without advancing the lexer and the cursor.
    //   2. Otherwise, if you don't have the current token, just call `self.parse_expr`.
    //      This one will obtain the current token and then forward that anyway.
    fn parse_expr(&mut self, pred: Precedence) -> Option<Expr> {
        let (_, tok) = self.next_non_ws_tok()?;
        self.parse_expr_by_tok(pred, tok)
    }

    fn parse_expr_by_tok(&mut self, pred: Precedence, tok: TokenId) -> Option<Expr> {
        match pred {
            Precedence::TyAnn => self.parse_ty_expr_by_tok(tok),
            Precedence::App => self.parse_app_expr_by_tok(tok),
            Precedence::Assoc => self.parse_assoc_expr_by_tok(tok),
            Precedence::Atom => self.parse_atom_expr_by_tok(tok),
        }
    }

    fn parse_ty_expr_by_tok(&mut self, tok: TokenId) -> Option<Expr> {
        let expr = self.parse_expr_by_tok(Precedence::App, tok)?;

        let Some((_, colon_tok)) = self.try_parse_operator(":") else {
            return Some(expr);
        };

        // Beautiful, beautiful Curry-Howard correspondence. No need for a whole
        // separate language for types. :)
        //
        // In practice though, we would obviously add in constraints, e.g. it
        // makes no sense to want `function` or `match` or `do` expressions in
        // the type level, but that's a semantic analysis problem because we
        // already have that problem in some cases anyway, consider the
        // following:
        //
        //   f : (x : Nat) -> (y : Nat) -> x = y
        //   f x x = Refl
        //
        // and also consider:
        //
        //   G : Type
        //   G = (x : Nat) -> (y : Nat) -> x = y
        //
        // Similarly, we also have a bit of the same problem even in the value
        // language:
        //
        //   id : a -> a
        //   id = forall x. x
        //
        // Also consider this one too. We'd like to make sure you're supposed to
        // have written the following:
        //
        //   id : a -> a
        //   id = \x -> x
        //
        // Or equivalently:
        //
        //   id : a -> a
        //   id x = x
        //
        // So we have quite a bit of work ahead of us, because either you have
        // an associative expression that does type ascription, or you have a Pi
        // type that introduces an `x` of type `Nat`.
        let ty = self.parse_expr(Precedence::App).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, colon_tok, colon_tok)
        });

        let last_tok = ty.end_token();
        Some(self.make_expr(ExprKind::ann(expr, ty), tok, last_tok))
    }

    fn parse_app_expr_by_tok(&mut self, tok: TokenId) -> Option<Expr> {
        match &self.tokens[tok].kind() {
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
            TokenKind::Group(Group::Paren(Parity::Opened)) => {
                self.parse_expr_by_tok(Precedence::Assoc, tok)
            }
            TokenKind::Group(Group::Brace(Parity::Opened)) => todo!(),
            TokenKind::Group(Group::Bracket(Parity::Opened)) => todo!(),
            TokenKind::Group(Group::Paren(Parity::Closed)) => None,
            TokenKind::Group(Group::Brace(Parity::Closed)) => None,
            TokenKind::Group(Group::Bracket(Parity::Closed)) => None,
            TokenKind::Ws(_) => todo!(),
            TokenKind::Semicolon => todo!(), // TODO: error,
            TokenKind::Comma => todo!(),     // TODO: error,
            TokenKind::Eof => None,
        }
    }

    fn parse_let_expr_tail(&mut self, let_tok: TokenId) -> Expr {
        let Some(f) = self.parse_expr(Precedence::TyAnn) else {
            return self.report_expr_error(SyntaxError::MissingExpr, let_tok, let_tok);
        };

        let Some((_, eq_tok)) = self.try_parse_operator("=") else {
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

        let Some((_, in_tok)) = self.try_parse_kw(Keyword::In) else {
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

        let mut last_tok = do_tok;
        while let Some(expr) = self.parse_expr(Precedence::TyAnn) {
            last_tok = expr.end_token();
            stmts.push(Stmt::expr(expr));
        }

        self.make_expr(ExprKind::do_block(stmts), do_tok, last_tok)
    }

    fn parse_if_expr_tail(&mut self, if_tok: TokenId) -> Expr {
        let antecedent = self
            .parse_expr(Precedence::TyAnn)
            .unwrap_or_else(|| self.report_expr_error(SyntaxError::MissingExpr, if_tok, if_tok));
        let then_tok = self.try_parse_kw(Keyword::Then);
        let last_tok = then_tok.as_ref().map(|(_, t)| t.clone()).unwrap_or(if_tok);
        let consequent = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report_expr_error(SyntaxError::MissingExpr, last_tok, last_tok)
        });
        let else_tok = self.try_parse_kw(Keyword::Else);
        let last_tok = else_tok
            .as_ref()
            .map(|(_, t)| t.clone())
            .unwrap_or(last_tok);
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

    fn parse_assoc_expr_by_tok(&mut self, open_paren: TokenId) -> Option<Expr> {
        // I could have used a loop here, but I didn't because there's some subtlety
        // wrt the way we track expression source spans, e.g. if we wrote the naive
        // loop, it might be the case that the expression
        //
        //   (((a) b) c)
        //
        // will have incorrect spans, e.g. the following diagram:
        //
        //   (((a) b) c)
        //   |   |  |  |
        //   +---+  |  |
        //   |      |  |
        //   +------+  |
        //   |         |
        //   +---------+
        //
        // which as we can see is incorrect. What we want to see is the following diagram:
        //
        //   (((a) b) c)
        //   ||| |  |  |
        //   ||+-+  |  |
        //   |+-----+  |
        //   +---------+
        //
        // So for now, we'll just do the dumb simple thing of recursion. If it becomes a
        // problem, I'll be motivated to fix it.
        let (offside, tok) = self.next_non_ws_tok()?;
        match self.tokens[tok].kind() {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(_) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::ByteString(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Group(Group::Paren(Parity::Opened)) => {
                let e = self.parse_expr(Precedence::Atom);
                match self.next_non_ws_tok() {
                    Some(tok) => todo!(),
                    None => todo!(),
                }
            }
            TokenKind::Group(Group::Brace(Parity::Opened)) => todo!(),
            TokenKind::Group(Group::Bracket(Parity::Opened)) => todo!(),
            TokenKind::Group(Group::Paren(Parity::Closed)) => None,
            TokenKind::Group(Group::Brace(Parity::Closed)) => None,
            TokenKind::Group(Group::Bracket(Parity::Closed)) => None,
            TokenKind::Ws(_) => todo!(),
            TokenKind::Semicolon => todo!(),
            TokenKind::Comma => todo!(),
            TokenKind::Eof => todo!(),
        }
    }

    fn parse_either_unit_or_expr_tail(&mut self, open_paren_tok: TokenId) -> Expr {
        if let Some((_, closed_paren_tok)) = self.try_parse_group(Group::Paren(Parity::Closed)) {
            return self.make_expr(ExprKind::unit(), open_paren_tok, closed_paren_tok);
        }

        let e = match self.next_non_ws_tok() {
            Some((_, tok)) => match self.tokens[tok].kind() {
                TokenKind::Group(Group::Paren(Parity::Closed)) => None,
                _ => self.parse_expr_by_tok(Precedence::TyAnn, tok),
            },
            None => {
                return self.report_expr_error(
                    SyntaxError::NotBalanced(Group::Paren(Parity::Closed)),
                    open_paren_tok,
                    open_paren_tok,
                );
            }
        };

        match e {
            Some(_) => todo!(),
            None => todo!(),
        }
    }

    fn parse_atom_expr_by_tok(&mut self, tok: TokenId) -> Option<Expr> {
        todo!()
    }
}
