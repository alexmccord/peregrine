use crate::syntax::cursor::{Cursor, Delimiter, Gate, Grapheme};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Module,
    Import,
    Struct,
    Data,
    Let,
    Do,
    In,
    Where,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Unknown(String),
    Kw(Keyword),
    Ident(String),
    Numeral(String),
    Operator(String),
    Delimiter(Delimiter),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    fn new(kind: TokenKind) -> Token {
        Token { kind }
    }

    fn unknown(str: &str) -> Token {
        Token::new(TokenKind::Unknown(str.to_string()))
    }

    fn kw(kw: Keyword) -> Token {
        Token::new(TokenKind::Kw(kw))
    }

    fn ident(ident: &str) -> Token {
        Token::new(TokenKind::Ident(ident.to_string()))
    }

    fn numeral(num: &str) -> Token {
        Token::new(TokenKind::Numeral(num.to_string()))
    }

    fn operator(op: &str) -> Token {
        Token::new(TokenKind::Operator(op.to_string()))
    }

    fn delimiter(delim: Delimiter) -> Token {
        Token::new(TokenKind::Delimiter(delim))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, TokenKind::Unknown(_))
    }

    pub fn is_kw(&self, kw: Keyword) -> bool {
        self.kind == TokenKind::Kw(kw)
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, TokenKind::Ident(_))
    }

    pub fn get_ident(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn is_numeral(&self) -> bool {
        matches!(self.kind, TokenKind::Numeral(_))
    }

    pub fn is_operator(&self) -> bool {
        matches!(self.kind, TokenKind::Operator(_))
    }

    pub fn get_op(&self) -> Option<&str> {
        match &self.kind {
            TokenKind::Operator(op) => Some(op),
            _ => None,
        }
    }

    pub fn is_opening_paren(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Paren(Gate::Opened))
    }

    pub fn is_closing_paren(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Paren(Gate::Closed))
    }

    pub fn is_opening_brace(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Brace(Gate::Opened))
    }

    pub fn is_closing_brace(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Brace(Gate::Closed))
    }

    pub fn is_opening_bracket(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Bracket(Gate::Opened))
    }

    pub fn is_closing_bracket(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Bracket(Gate::Closed))
    }
}

pub enum ScanCommand {
    Append,
    Terminate,
    Error,
}

pub struct Lexer {
    cursor: Cursor,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
        }
    }

    fn scan(&mut self, f: fn(Grapheme) -> ScanCommand) -> Result<&str, Token> {
        let i = self.cursor.offset();

        loop {
            let j = self.cursor.offset();

            match f(self.cursor.get()) {
                ScanCommand::Append => {
                    self.cursor.next();
                }
                ScanCommand::Terminate => return Ok(self.cursor.slice(i, j)),
                ScanCommand::Error => return Err(Token::unknown(self.cursor.slice(i, j))),
            }
        }
    }

    fn scan_unknown(&mut self) -> Token {
        let res = self.scan(|c| match c {
            Grapheme::Unknown(_) => ScanCommand::Append,
            _ => ScanCommand::Terminate,
        });

        match res {
            Ok(str) => Token::unknown(str),
            Err(tok) => tok,
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanCommand::Append,
            Grapheme::Digit(_) => ScanCommand::Append,
            Grapheme::Operator(_) => ScanCommand::Terminate,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Terminate,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        match res {
            Ok(str) => {
                if let Some(kw) = Lexer::map_to_keyword(&str) {
                    Token::kw(kw)
                } else {
                    Token::ident(str)
                }
            }
            Err(tok) => tok,
        }
    }

    fn map_to_keyword(ident: &str) -> Option<Keyword> {
        match ident {
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "struct" => Some(Keyword::Struct),
            "data" => Some(Keyword::Data),
            "let" => Some(Keyword::Let),
            "do" => Some(Keyword::Do),
            "in" => Some(Keyword::In),
            "where" => Some(Keyword::Where),
            _ => None,
        }
    }

    fn scan_numeral(&mut self) -> Token {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanCommand::Error,
            Grapheme::Digit(_) => ScanCommand::Append,
            Grapheme::Operator(_) => ScanCommand::Terminate,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Terminate,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        match res {
            Ok(str) => Token::numeral(str),
            Err(tok) => tok,
        }
    }

    fn scan_operator(&mut self) -> Token {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanCommand::Terminate,
            Grapheme::Digit(_) => ScanCommand::Terminate,
            Grapheme::Operator(_) => ScanCommand::Append,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Error,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        match res {
            Ok(str) => Token::operator(str),
            Err(tok) => tok,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        match self.cursor.get() {
            Grapheme::Unknown(_) => Some(self.scan_unknown()),
            Grapheme::Alpha(_) => Some(self.scan_identifier()),
            Grapheme::Digit(_) => Some(self.scan_numeral()),
            Grapheme::Operator(_) => Some(self.scan_operator()),
            Grapheme::Delimiter(delim) => {
                self.cursor.next();
                Some(Token::delimiter(delim))
            }
            Grapheme::Space(_) => {
                self.cursor.next();
                self.next()
            }
            Grapheme::Newline(_) => {
                self.cursor.next();
                self.next()
            }
            Grapheme::Eof => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Delimiter, Gate, Keyword, Lexer, Token};

    #[test]
    fn scan_unknown() {
        let mut lexer = Lexer::new("@@@");
        assert_eq!(lexer.next(), Some(Token::unknown("@@@")));
    }

    #[test]
    fn scan_kw() {
        let mut kws = Vec::new();
        kws.push(("module", Keyword::Module));
        kws.push(("import", Keyword::Import));
        kws.push(("struct", Keyword::Struct));
        kws.push(("data", Keyword::Data));
        kws.push(("let", Keyword::Let));
        kws.push(("do", Keyword::Do));
        kws.push(("in", Keyword::In));
        kws.push(("where", Keyword::Where));

        for (str, kw) in kws {
            let mut lexer = Lexer::new(str);
            assert_eq!(lexer.next(), Some(Token::kw(kw)));
        }
    }

    #[test]
    fn scan_ident() {
        let mut lexer = Lexer::new("abc");
        assert_eq!(lexer.next(), Some(Token::ident("abc")));
    }

    #[test]
    fn scan_ident_with_numerals() {
        let mut lexer = Lexer::new("abc12");
        assert_eq!(lexer.next(), Some(Token::ident("abc12")));
    }

    #[test]
    fn identifiers_dont_start_with_digits() {
        let mut lexer = Lexer::new("1abc");
        assert_eq!(lexer.next(), Some(Token::unknown("1")));
    }

    #[test]
    fn multiple_tokens() {
        let mut lexer = Lexer::new("abc 123");
        assert_eq!(lexer.next(), Some(Token::ident("abc")));
        assert_eq!(lexer.next(), Some(Token::numeral("123")));
    }

    #[test]
    fn scan_operators() {
        let mut lexer = Lexer::new(". .. .| ~ && ~()");
        assert_eq!(lexer.next(), Some(Token::operator(".")));
        assert_eq!(lexer.next(), Some(Token::operator("..")));
        assert_eq!(lexer.next(), Some(Token::operator(".|")));
        assert_eq!(lexer.next(), Some(Token::operator("~")));
        assert_eq!(lexer.next(), Some(Token::operator("&&")));
        assert_eq!(lexer.next(), Some(Token::operator("~")));
        assert_eq!(
            lexer.next(),
            Some(Token::delimiter(Delimiter::Paren(Gate::Opened)))
        );
        assert_eq!(
            lexer.next(),
            Some(Token::delimiter(Delimiter::Paren(Gate::Closed)))
        );
        assert_eq!(lexer.next(), None);
    }

    // #[test]
    // fn scan_indentation() {
    //     let mut lexer = Lexer::new("\na\n b\n  c\n d\ne");
    //     assert_eq!(lexer.next(), Some(Token::ident("a")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("b")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("c")));
    //     assert_eq!(lexer.next(), Some(Token::dedent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("d")));
    //     assert_eq!(lexer.next(), Some(Token::dedent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("e")));
    //     assert_eq!(lexer.next(), None);
    // }

    // #[test]
    // fn indent_token_are_emitted_only_if_indentation_changes() {
    //     let mut lexer = Lexer::new("a\n  b\n  c\n  \td\n  \t  e");
    //     assert_eq!(lexer.next(), Some(Token::ident("a")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("b")));
    //     assert_eq!(lexer.next(), Some(Token::ident("c")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("d")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("e")));
    //     assert_eq!(lexer.next(), None);
    // }

    // #[test]
    // fn no_mixed_indentation() {
    //     let mut lexer = Lexer::new("a\n  b\n\t\tc");
    //     assert_eq!(lexer.next(), Some(Token::ident("a")));
    //     assert_eq!(lexer.next(), Some(Token::indent()));
    //     assert_eq!(lexer.next(), Some(Token::ident("b")));
    //     assert_eq!(lexer.next(), Some(Token::unknown("\t\t")));
    //     assert_eq!(lexer.next(), Some(Token::ident("c")));
    //     assert_eq!(lexer.next(), None);
    // }
}
