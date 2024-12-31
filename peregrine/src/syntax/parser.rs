use crate::syntax::ast;
use crate::syntax::lexer::{Keyword, Lexer, Token, TokenKind};

use super::ast::Symbol;

pub enum ParseError {
    MissingName,
    MissingExpr,
}

pub enum ParseResult<'ast> {
    Success {
        arena: ast::AstAllocator<'ast>,
        program: ast::Program<'ast>,
    },
    Invalid {
        arena: ast::AstAllocator<'ast>,
        program: ast::Program<'ast>,
        errors: Vec<ParseError>,
    },
}

impl<'ast> ParseResult<'ast> {
    fn get_arena(&self) -> &ast::AstAllocator<'ast> {
        match self {
            ParseResult::Success { arena, program: _ } => &arena,
            ParseResult::Invalid {
                arena,
                program: _,
                errors: _,
            } => &arena,
        }
    }

    pub fn get_program(&self) -> &ast::Program<'ast> {
        match self {
            ParseResult::Success { arena: _, program } => &program,
            ParseResult::Invalid {
                arena: _,
                program,
                errors: _,
            } => &program,
        }
    }

    pub fn get_expr(&self, id: ast::ExprId<'ast>) -> &ast::Expr<'ast> {
        self.get_arena().get_expr(id)
    }

    pub fn get_decl(&self, id: ast::DeclId<'ast>) -> &ast::Decl<'ast> {
        self.get_arena().get_decl(id)
    }
}

impl<'ast> ParseResult<'ast> {
    fn success(arena: ast::AstAllocator<'ast>, program: ast::Program<'ast>) -> ParseResult<'ast> {
        ParseResult::Success { arena, program }
    }

    fn invalid(
        arena: ast::AstAllocator<'ast>,
        program: ast::Program<'ast>,
        errors: Vec<ParseError>,
    ) -> ParseResult<'ast> {
        ParseResult::Invalid {
            arena,
            program,
            errors,
        }
    }
}

pub enum Precedence {
    TyAnn, // e : T
    App,   // f e
    Atom,  // e
}

pub struct Parser<'ast> {
    lexer: Lexer,
    arena: ast::AstAllocator<'ast>,
    errors: Vec<ParseError>,
}

impl<'ast> Parser<'ast> {
    fn new(input: String) -> Parser<'ast> {
        Parser {
            lexer: Lexer::new(input),
            arena: ast::AstAllocator::default(),
            errors: Vec::default(),
        }
    }

    fn next(&mut self) -> Option<Token> {
        // TODO: This function will skip comments and other random trivias.
        // Trivias are things that has to be emitted, but are otherwise
        // not significant with respect to the parser. Since Peregrine is
        // whitespace sensitive, indent/dedent/newlines are significant,
        // and thus are not trivias.
        self.lexer.next()
    }

    fn try_consume<F>(&mut self, f: F) -> Option<Token>
    where
        F: FnOnce(&Token) -> bool,
    {
        if f(self.lookahead()?) {
            self.next()
        } else {
            None
        }
    }

    fn lookahead(&mut self) -> Option<&Token> {
        // TODO: Same problem, skip trivias.
        self.lexer.lookahead()
    }

    fn report(&mut self, err: ParseError) {
        self.errors.push(err);
    }

    pub fn parse(input: String) -> ParseResult<'ast> {
        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            ParseResult::success(parser.arena, program)
        } else {
            ParseResult::invalid(parser.arena, program, parser.errors)
        }
    }

    fn parse_program(&mut self) -> ast::Program<'ast> {
        let mut program = ast::Program::new();

        while let Some(decl) = self.parse_top_level() {
            program.push(decl);
        }

        program
    }

    fn parse_top_level(&mut self) -> Option<ast::DeclId<'ast>> {
        self.next().map(|tok| match tok.kind {
            TokenKind::Unknown(_) => todo!(),
            TokenKind::Kw(Keyword::Module) => todo!(),
            TokenKind::Kw(Keyword::Import) => self.parse_import_decl(&tok),
            TokenKind::Kw(Keyword::Struct) => todo!(),
            TokenKind::Kw(Keyword::Data) => todo!(),
            TokenKind::Kw(Keyword::Let) => self.parse_let_decl(&tok),
            TokenKind::Kw(Keyword::Do) => todo!(),
            TokenKind::Ident(_) => todo!(),
            TokenKind::Numeral(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Delimiter(_) => todo!(),
            TokenKind::Indent => todo!(),
            TokenKind::Dedent => todo!(),
        })
    }

    fn parse_import_decl(&mut self, tok: &Token) -> ast::DeclId<'ast> {
        assert!(Token::is_kw(tok, Keyword::Import));

        let mut path = Vec::new();

        while let Some(tok) = self.next() {
            if let TokenKind::Ident(id) = tok.kind {
                path.push(id)
            }

            if self.try_parse_operator('.').is_none() {
                break;
            }

            self.next(); // consumes lookahead token
        }

        self.arena
            .alloc_decl(ast::Decl::Import(ast::Import { path }))
    }

    fn parse_let_decl(&mut self, tok: &Token) -> ast::DeclId<'ast> {
        assert!(Token::is_kw(tok, Keyword::Let));

        let f = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
            self.report(ParseError::MissingExpr);
            self.arena.alloc_expr(ast::Expr::Error)
        });

        if self.try_parse_operator('=').is_some() {
            let e = self.parse_expr(Precedence::TyAnn).unwrap_or_else(|| {
                self.report(ParseError::MissingExpr);
                self.arena.alloc_expr(ast::Expr::Error)
            });

            self.arena
                .alloc_decl(ast::Decl::Let(ast::Let::Assign(f, e)))
        } else {
            self.arena.alloc_decl(ast::Decl::Let(ast::Let::Decl(f)))
        }
    }

    fn try_parse_operator(&mut self, c: char) -> Option<Token> {
        self.try_consume(|tok| match &tok.kind {
            TokenKind::Operator(s) => s.as_bytes() == [c as u8],
            _ => false,
        })
    }

    fn parse_expr(&mut self, pred: Precedence) -> Option<ast::ExprId<'ast>> {
        match pred {
            Precedence::TyAnn => {
                let expr = self
                    .parse_expr(Precedence::App)
                    .unwrap_or_else(|| self.arena.alloc_expr(ast::Expr::Error)); // missed opportunity to report error

                if self.try_parse_operator(':').is_none() {
                    return Some(expr);
                };

                // Beautiful, beautiful Curry-Howard correspondence.
                // No need for a whole separate language for types. :)
                let ty = self
                    .parse_expr(Precedence::App)
                    .unwrap_or_else(|| self.arena.alloc_expr(ast::Expr::Error)); // missed opportunity to report error

                Some(self.arena.alloc_expr(ast::Expr::Ann(expr, ty)))
            }
            Precedence::App => todo!(),
            Precedence::Atom => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseResult, Parser};

    #[test]
    fn parse_nothing() {
        let result = Parser::parse("".to_string());
        assert!(matches!(result, ParseResult::Success { .. }))
    }

    #[test]
    fn parse_import_decl() {
        let result = Parser::parse("import A.B.C".to_string());
        let program = result.get_program();
        assert_eq!(program.decls.len(), 1);

        let import_decl_id = program.decls[0];
        let import_decl = result.get_decl(import_decl_id).get_import().unwrap();
        assert_eq!(import_decl.path, vec!["A", "B", "C"]);
    }

    #[test]
    fn parse_let_decl() {
        let result = Parser::parse("let id x = x".to_string());
        let program = result.get_program();
        assert_eq!(program.decls.len(), 1);

        let let_decl_id = program.decls[0];
        let let_decl = result.get_decl(let_decl_id).get_let().unwrap();
    }
}
