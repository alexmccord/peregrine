use crate::ast::Position;
use crate::idx;
use crate::syn::cursor::{Cursor, Delimiter, Gate, Grapheme};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    // Module system
    Module,
    Import,
    Export,
    Public,
    Open,
    Hiding,
    Renaming,

    // Product and sum types
    Struct,
    Data,
    Deriving,

    // Typeclasses
    Class,
    Instance,

    // Syntactic delimiters
    Where,

    // Expressions
    Let,
    In,
    Do,
    If,
    Then,
    Else,
    Function,
    Match,
    With,
    Forall,
    Exists,
}

impl Keyword {
    pub fn find(s: impl AsRef<str>) -> Option<Keyword> {
        match s.as_ref() {
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "export" => Some(Keyword::Export),
            "public" => Some(Keyword::Public),
            "open" => Some(Keyword::Open),
            "hiding" => Some(Keyword::Hiding),
            "renaming" => Some(Keyword::Renaming),
            "struct" => Some(Keyword::Struct),
            "data" => Some(Keyword::Data),
            "class" => Some(Keyword::Class),
            "instance" => Some(Keyword::Instance),
            "deriving" => Some(Keyword::Deriving),
            "where" => Some(Keyword::Where),
            "let" => Some(Keyword::Let),
            "in" => Some(Keyword::In),
            "do" => Some(Keyword::Do),
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "function" => Some(Keyword::Function),
            "match" => Some(Keyword::Match),
            "with" => Some(Keyword::With),
            "forall" => Some(Keyword::Forall),
            "exists" => Some(Keyword::Exists),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Keyword::Module => "module",
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Public => "public",
            Keyword::Open => "open",
            Keyword::Hiding => "hiding",
            Keyword::Renaming => "renaming",
            Keyword::Struct => "struct",
            Keyword::Data => "data",
            Keyword::Deriving => "deriving",
            Keyword::Class => "class",
            Keyword::Instance => "instance",
            Keyword::Where => "where",
            Keyword::Let => "let",
            Keyword::In => "in",
            Keyword::Do => "do",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Function => "function",
            Keyword::Match => "match",
            Keyword::With => "with",
            Keyword::Forall => "forall",
            Keyword::Exists => "exists",
        }
    }

    pub fn len(&self) -> usize {
        self.to_str().len()
    }
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

pub type TokenId = idx::Id<Token>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    id: TokenId,
    kind: TokenKind,
    // The _beginning_ of the token. The _end_ can be computed.
    // To access them, use `tok.begin()` and `tok.end()`
    pos: Position,
}

impl Token {
    pub fn id(&self) -> TokenId {
        self.id
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn begin(&self) -> Position {
        self.pos
    }

    pub fn end(&self) -> Position {
        let len = match &self.kind {
            TokenKind::Unknown(s) => s.len(),
            TokenKind::Kw(kw) => kw.len(),
            TokenKind::Ident(s) => s.len(),
            TokenKind::Numeral(s) => s.len(),
            TokenKind::Operator(s) => s.len(),
            TokenKind::Delimiter(d) => d.len(),
        };

        Position::new(self.pos.line(), self.pos.column() + len)
    }

    pub fn is_unknown(&self, str: impl AsRef<str>) -> bool {
        matches!(&self.kind, TokenKind::Unknown(val) if val == str.as_ref())
    }

    pub fn is_kw(&self, kw: Keyword) -> bool {
        self.kind == TokenKind::Kw(kw)
    }

    pub fn is_ident(&self, ident: impl AsRef<str>) -> bool {
        matches!(&self.kind, TokenKind::Ident(val) if val == ident.as_ref())
    }

    pub fn get_ident(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn is_numeral(&self, num: impl AsRef<str>) -> bool {
        matches!(&self.kind, TokenKind::Numeral(val) if val == num.as_ref())
    }

    pub fn is_operator(&self, op: impl AsRef<str>) -> bool {
        matches!(&self.kind, TokenKind::Operator(val) if val == op.as_ref())
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

    pub fn is_semicolon(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Semicolon)
    }

    pub fn is_comma(&self) -> bool {
        self.kind == TokenKind::Delimiter(Delimiter::Comma)
    }

    fn new(id: TokenId, kind: TokenKind, pos: Position) -> Token {
        Token { id, kind, pos }
    }

    fn unknown(id: TokenId, str: impl Into<String>, pos: Position) -> Token {
        Token::new(id, TokenKind::Unknown(str.into()), pos)
    }

    fn kw(id: TokenId, kw: Keyword, pos: Position) -> Token {
        Token::new(id, TokenKind::Kw(kw), pos)
    }

    fn ident(id: TokenId, ident: impl Into<String>, pos: Position) -> Token {
        Token::new(id, TokenKind::Ident(ident.into()), pos)
    }

    fn numeral(id: TokenId, num: impl Into<String>, pos: Position) -> Token {
        Token::new(id, TokenKind::Numeral(num.into()), pos)
    }

    fn operator(id: TokenId, op: impl Into<String>, pos: Position) -> Token {
        Token::new(id, TokenKind::Operator(op.into()), pos)
    }

    fn delimiter(id: TokenId, delim: Delimiter, pos: Position) -> Token {
        Token::new(id, TokenKind::Delimiter(delim), pos)
    }
}

pub enum ScanCommand {
    Append,
    Terminate,
    Error,
}

pub struct Lexer {
    cursor: Cursor,
    current_pos: Position,
    ids: idx::Generation<Token>,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
            current_pos: Position::new(1, 0),
            ids: idx::Generation::new(),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            let pos = self.current_pos;

            return match self.cursor.get() {
                Grapheme::Unknown(_) => Some(self.scan_unknown(pos)),
                Grapheme::Alpha(_) => Some(self.scan_identifier(pos)),
                Grapheme::Digit(_) => Some(self.scan_numeral(pos)),
                Grapheme::Operator(_) => Some(self.scan_operator(pos)),
                Grapheme::Delimiter(delim) => {
                    self.advance_cursor();
                    Some(Token::delimiter(self.ids.next(), delim, pos))
                }
                Grapheme::Space(_) => {
                    self.advance_cursor();
                    continue;
                }
                Grapheme::Newline(_) => {
                    self.advance_cursor();
                    continue;
                }
                Grapheme::Eof => None,
            };
        }
    }

    pub fn current_pos(&self) -> Position {
        self.current_pos
    }

    fn scan(
        &mut self,
        pos: Position,
        f: impl Fn(Grapheme) -> ScanCommand,
    ) -> Result<String, Token> {
        let i = self.cursor.offset();
        let mut ok = true;

        loop {
            let c = self.cursor.get();
            let j = self.cursor.offset();

            match f(c) {
                ScanCommand::Append => {
                    self.advance_cursor();
                    continue;
                }
                ScanCommand::Terminate => {
                    return if ok {
                        Ok(self.cursor.slice(i, j).to_owned())
                    } else {
                        Err(Token::unknown(
                            self.ids.next(),
                            self.cursor.slice(i, j),
                            pos,
                        ))
                    }
                }
                ScanCommand::Error => {
                    self.advance_cursor();
                    ok = false;
                    continue;
                }
            };
        }
    }

    fn scan_unknown(&mut self, pos: Position) -> Token {
        let res = self.scan(pos, |c| match c {
            Grapheme::Unknown(_) => ScanCommand::Append,
            _ => ScanCommand::Terminate,
        });

        res.map_or_else(|tok| tok, |str| Token::unknown(self.ids.next(), str, pos))
    }

    fn scan_identifier(&mut self, pos: Position) -> Token {
        let res = self.scan(pos, |c| match c {
            Grapheme::Alpha(_) => ScanCommand::Append,
            Grapheme::Digit(_) => ScanCommand::Append,
            Grapheme::Operator(_) => ScanCommand::Terminate,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Terminate,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        res.map_or_else(
            |tok| tok,
            |str| {
                Keyword::find(&str).map_or(Token::ident(self.ids.next(), str, pos), |kw| {
                    Token::kw(self.ids.next(), kw, pos)
                })
            },
        )
    }

    fn scan_numeral(&mut self, pos: Position) -> Token {
        let res = self.scan(pos, |c| match c {
            Grapheme::Alpha(_) => ScanCommand::Error,
            Grapheme::Digit(_) => ScanCommand::Append,
            Grapheme::Operator(_) => ScanCommand::Terminate,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Terminate,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        res.map_or_else(|tok| tok, |str| Token::numeral(self.ids.next(), str, pos))
    }

    fn scan_operator(&mut self, pos: Position) -> Token {
        let res = self.scan(pos, |c| match c {
            Grapheme::Alpha(_) => ScanCommand::Terminate,
            Grapheme::Digit(_) => ScanCommand::Terminate,
            Grapheme::Operator(_) => ScanCommand::Append,
            Grapheme::Delimiter(_) => ScanCommand::Terminate,
            Grapheme::Space(_) => ScanCommand::Terminate,
            Grapheme::Newline(_) => ScanCommand::Terminate,
            Grapheme::Unknown(_) => ScanCommand::Error,
            Grapheme::Eof => ScanCommand::Terminate,
        });

        res.map_or_else(|tok| tok, |str| Token::operator(self.ids.next(), str, pos))
    }

    fn advance_cursor(&mut self) {
        match self.cursor.next() {
            Some(Grapheme::Newline(..)) => {
                self.current_pos = Position::new(self.current_pos.line() + 1, 0);
            }
            Some(c) => {
                self.current_pos =
                    Position::new(self.current_pos.line(), self.current_pos.column() + c.len())
            }
            None => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_unknown() {
        let mut lexer = Lexer::new("@@@");

        let tok = lexer.next_token().unwrap();
        assert!(tok.is_unknown("@@@"));
        assert!(tok.begin() == Position::new(1, 0));
        assert!(tok.end() == Position::new(1, 3));

        assert!(lexer.next_token().is_none());
    }

    #[test]
    fn scan_kw() {
        let kws = vec![
            ("module", Keyword::Module),
            ("import", Keyword::Import),
            ("export", Keyword::Export),
            ("public", Keyword::Public),
            ("open", Keyword::Open),
            ("hiding", Keyword::Hiding),
            ("renaming", Keyword::Renaming),
            ("struct", Keyword::Struct),
            ("data", Keyword::Data),
            ("class", Keyword::Class),
            ("instance", Keyword::Instance),
            ("deriving", Keyword::Deriving),
            ("where", Keyword::Where),
            ("let", Keyword::Let),
            ("in", Keyword::In),
            ("do", Keyword::Do),
            ("if", Keyword::If),
            ("then", Keyword::Then),
            ("else", Keyword::Else),
            ("function", Keyword::Function),
            ("match", Keyword::Match),
            ("with", Keyword::With),
            ("forall", Keyword::Forall),
            ("exists", Keyword::Exists),
        ];

        for (str, kw) in kws {
            let mut lexer = Lexer::new(str);

            let tok = lexer.next_token().unwrap();
            assert!(tok.is_kw(kw));
            assert!(tok.begin() == Position::new(1, 0));
            assert!(tok.end() == Position::new(1, str.len()));

            assert!(lexer.next_token().is_none());
        }
    }

    #[test]
    fn scan_ident() {
        let mut lexer = Lexer::new("abc");

        let tok = lexer.next_token().unwrap();
        assert!(tok.is_ident("abc"));
        assert!(tok.begin() == Position::new(1, 0));
        assert!(tok.end() == Position::new(1, 3));

        assert!(lexer.next_token().is_none());
    }

    #[test]
    fn scan_ident_with_numerals() {
        let mut lexer = Lexer::new("abc12");

        let tok = lexer.next_token().unwrap();
        assert!(tok.is_ident("abc12"));
        assert!(tok.begin() == Position::new(1, 0));
        assert!(tok.end() == Position::new(1, 5));

        assert!(lexer.next_token().is_none());
    }

    #[test]
    fn identifiers_dont_start_with_digits() {
        let mut lexer = Lexer::new("1abc");

        let tok = lexer.next_token().unwrap();
        assert!(tok.is_unknown("1abc"));
        assert!(tok.begin() == Position::new(1, 0));
        assert!(tok.end() == Position::new(1, 4));

        assert!(lexer.next_token().is_none());
    }

    #[test]
    fn multiple_tokens() {
        let mut lexer = Lexer::new("abc 123");

        let tok1 = lexer.next_token().unwrap();
        assert!(tok1.is_ident("abc"));
        assert!(tok1.begin() == Position::new(1, 0));
        assert!(tok1.end() == Position::new(1, 3));

        let tok2 = lexer.next_token().unwrap();
        assert!(tok2.is_numeral("123"));
        assert!(tok2.begin() == Position::new(1, 4));
        assert!(tok2.end() == Position::new(1, 7));
    }

    #[test]
    fn scan_operators() {
        let mut lexer = Lexer::new(". .. .| ~ && ~()");

        let tok1 = lexer.next_token().unwrap();
        assert!(tok1.is_operator("."));
        assert!(tok1.begin() == Position::new(1, 0));
        assert!(tok1.end() == Position::new(1, 1));

        let tok2 = lexer.next_token().unwrap();
        assert!(tok2.is_operator(".."));
        assert!(tok2.begin() == Position::new(1, 2));
        assert!(tok2.end() == Position::new(1, 4));

        let tok3 = lexer.next_token().unwrap();
        assert!(tok3.is_operator(".|"));
        assert!(tok3.begin() == Position::new(1, 5));
        assert!(tok3.end() == Position::new(1, 7));

        let tok4 = lexer.next_token().unwrap();
        assert!(tok4.is_operator("~"));
        assert!(tok4.begin() == Position::new(1, 8));
        assert!(tok4.end() == Position::new(1, 9));

        let tok5 = lexer.next_token().unwrap();
        assert!(tok5.is_operator("&&"));
        assert!(tok5.begin() == Position::new(1, 10));
        assert!(tok5.end() == Position::new(1, 12));

        let tok6 = lexer.next_token().unwrap();
        assert!(tok6.is_operator("~"));
        assert!(tok6.begin() == Position::new(1, 13));
        assert!(tok6.end() == Position::new(1, 14));

        let tok7 = lexer.next_token().unwrap();
        assert!(tok7.is_opening_paren());
        assert!(tok7.begin() == Position::new(1, 14));
        assert!(tok7.end() == Position::new(1, 15));

        let tok8 = lexer.next_token().unwrap();
        assert!(tok8.is_closing_paren());
        assert!(tok8.begin() == Position::new(1, 15));
        assert!(tok8.end() == Position::new(1, 16));

        assert!(lexer.next_token().is_none());
    }

    #[test]
    fn scan_with_newlines() {
        let mut lexer = Lexer::new("abc def\nghi jkl");

        let tok1 = lexer.next_token().unwrap();
        assert!(tok1.is_ident("abc"));
        assert!(tok1.begin() == Position::new(1, 0));
        assert!(tok1.end() == Position::new(1, 3));

        let tok2 = lexer.next_token().unwrap();
        assert!(tok2.is_ident("def"));
        assert!(tok2.begin() == Position::new(1, 4));
        assert!(tok2.end() == Position::new(1, 7));

        let tok3 = lexer.next_token().unwrap();
        assert!(tok3.is_ident("ghi"));
        assert!(tok3.begin() == Position::new(2, 0));
        assert!(tok3.end() == Position::new(2, 3));

        let tok4 = lexer.next_token().unwrap();
        assert!(tok4.is_ident("jkl"));
        assert!(tok4.begin() == Position::new(2, 4));
        assert!(tok4.end() == Position::new(2, 7));

        assert!(lexer.next_token().is_none());
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
