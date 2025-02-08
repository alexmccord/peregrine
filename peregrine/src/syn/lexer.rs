use std::ops::Index;

use crate::idx;

use crate::ast::Position;
use crate::syn::cursor::{Cursor, Delimiter, Grapheme, Operator, Quotation};

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

// Lexer lexes this into a raw representation of a string literal as it exists
// in the source code. It is not the responsibility of the lexer to parse it out
// to give it the actual semantic meaning.
//
// In fact, we want to prevent this from happening at the compiler level
// entirely. This should be pushed down to the language level. It does mean that
// string literals have no principal types, but we already have that problem in
// other cases anyway.
//
// Be aware of the following:
// 1. this includes the quotation marks in all cases.
// 2. in the case of ByteChar, the length of the string is still arbitrary.
// 3. using `len()` returns the number of bytes of the source code, not the span.
//
// We also have to store `Position` in `ByteString` since it contains multiline
// string literals, and all other `TokenKind` can only span one line anyway.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ByteString {
    ByteString(String, Position),
    ByteChar(String, Position),
}

impl ByteString {
    pub fn new_bytestring(str: impl Into<String>, end_pos: Position) -> ByteString {
        ByteString::ByteString(str.into(), end_pos)
    }

    pub fn new_bytechar(str: impl Into<String>, end_pos: Position) -> ByteString {
        ByteString::ByteChar(str.into(), end_pos)
    }

    pub fn from_quot(quot: Quotation, str: impl Into<String>, end_pos: Position) -> ByteString {
        match quot {
            Quotation::Single => ByteString::new_bytechar(str, end_pos),
            Quotation::Double => ByteString::new_bytestring(str, end_pos),
        }
    }

    /// Returns the length of the string including the quotations.
    pub fn len(&self) -> usize {
        match self {
            ByteString::ByteString(str, _) => str.len(),
            ByteString::ByteChar(str, _) => str.len(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Unknown(String),
    Kw(Keyword),
    Ident(String),
    Numeral(String),
    ByteString(ByteString),
    Operator(String),
    Delimiter(Delimiter),
}

idx::newindex!(pub TokenId);

pub type TokenVec = idx::IndexedVec<TokenId, Token>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    kind: TokenKind,
    // The _beginning_ of the token. The _end_ can be computed.
    // To access them, use `tok.begin()` and `tok.end()`
    pos: Position,
}

impl Token {
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
            TokenKind::ByteString(ByteString::ByteString(_, end)) => return end.clone(),
            TokenKind::ByteString(ByteString::ByteChar(_, end)) => return end.clone(),
            TokenKind::Operator(s) => s.len(),
            TokenKind::Delimiter(d) => d.len(),
        };

        Position::new(self.pos.line(), self.pos.column() + len)
    }

    fn new(kind: TokenKind, pos: Position) -> Token {
        Token { kind, pos }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ScanResult {
    Ok(ScanCommand),
    Err(ScanCommand),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ScanCommand {
    Append,
    Terminate,
}

use ScanCommand::{Append, Terminate};

fn map_tok<F>(res: Result<String, TokenKind>, f: F) -> TokenKind
where
    F: FnOnce(String) -> TokenKind,
{
    res.map_or_else(|tok| tok, |str| f(str))
}

#[derive(Debug)]
pub(crate) struct Lexer {
    cursor: Cursor,
    current_pos: Position,
    tokens: TokenVec,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
            current_pos: Position::new(1, 0),
            tokens: idx::IndexedVec::new(),
        }
    }

    pub fn current_position(&self) -> Position {
        self.current_pos
    }

    fn advance_cursor(&mut self) -> bool {
        match self.cursor.next() {
            Some(Grapheme::Newline(..)) => {
                self.current_pos = Position::new(self.current_pos.line() + 1, 0);
                true
            }
            Some(c) => {
                let (line, col) = (self.current_pos.line(), self.current_pos.column() + c.len());
                self.current_pos = Position::new(line, col);
                true
            }
            None => false,
        }
    }

    fn scan(&mut self, f: impl Fn(Grapheme) -> ScanResult) -> Result<String, TokenKind> {
        self.scan_with((), |(), g| ((), f(g)))
    }

    fn scan_with<T>(
        &mut self,
        mut state: T,
        f: impl Fn(T, Grapheme) -> (T, ScanResult),
    ) -> Result<String, TokenKind> {
        let i = self.cursor.offset();
        let mut ok = true;

        // We call `self.advance_cursor()` here and now without the loop because
        // `self.next()` already checked what grapheme we have so we can skip that
        // redundant computation. But we have to do this after we get our `i`,
        // otherwise slicing will be off by one byte at the start.
        self.advance_cursor();

        loop {
            let (s, res) = f(state, self.cursor.get());
            state = s;

            let command = match res {
                ScanResult::Ok(cmd) => cmd,
                ScanResult::Err(cmd) => {
                    ok = false;
                    cmd
                }
            };

            match command {
                Append => {
                    self.advance_cursor();
                    continue;
                }
                Terminate => {
                    let j = self.cursor.offset();
                    let str = self.cursor.slice(i, j).to_string();

                    if ok {
                        break Ok(str);
                    } else {
                        break Err(TokenKind::Unknown(str));
                    }
                }
            };
        }
    }

    fn scan_unknown(&mut self) -> TokenKind {
        // We don't really _need_ for scan to return a Result
        // in this case, but... why duplicate logic? /shrug.
        let res = self.scan(|c| match c {
            Grapheme::Unknown(_) => ScanResult::Err(Append),
            _ => ScanResult::Err(Terminate),
        });

        map_tok(res, |str| TokenKind::Unknown(str))
    }

    fn scan_identifier(&mut self) -> TokenKind {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanResult::Ok(Append),
            Grapheme::Digit(_) => ScanResult::Ok(Append),
            Grapheme::Quot(_) => ScanResult::Ok(Append), // TODO: enforce it to be postfix?
            Grapheme::Operator(_) => ScanResult::Ok(Terminate),
            Grapheme::Delimiter(_) => ScanResult::Ok(Terminate),
            Grapheme::Space(_) => ScanResult::Ok(Terminate),
            Grapheme::Newline(_) => ScanResult::Ok(Terminate),
            Grapheme::Unknown(_) => ScanResult::Ok(Terminate),
            Grapheme::Eof => ScanResult::Ok(Terminate),
        });

        map_tok(res, |str| match Keyword::find(&str) {
            Some(kw) => TokenKind::Kw(kw),
            None => TokenKind::Ident(str),
        })
    }

    fn scan_numeral(&mut self) -> TokenKind {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanResult::Err(Append),
            Grapheme::Digit(_) => ScanResult::Ok(Append),
            Grapheme::Quot(_) => ScanResult::Err(Terminate),
            Grapheme::Operator(_) => ScanResult::Ok(Terminate),
            Grapheme::Delimiter(_) => ScanResult::Ok(Terminate),
            Grapheme::Space(_) => ScanResult::Ok(Terminate),
            Grapheme::Newline(_) => ScanResult::Ok(Terminate),
            Grapheme::Unknown(_) => ScanResult::Ok(Terminate),
            Grapheme::Eof => ScanResult::Ok(Terminate),
        });

        map_tok(res, |str| TokenKind::Numeral(str))
    }

    fn scan_bytestring(&mut self, quot: Quotation) -> TokenKind {
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        enum State {
            Escaped,
            Next,
            Finished,
        }

        use State::*;

        let res = self.scan_with(Next, |s, g| match (s, g) {
            (Finished, _) => (Finished, ScanResult::Ok(Terminate)),
            (_, Grapheme::Alpha(_)) => (Next, ScanResult::Ok(Append)),
            (_, Grapheme::Digit(_)) => (Next, ScanResult::Ok(Append)),
            (Next, Grapheme::Quot(q)) if q == quot => (Finished, ScanResult::Ok(Append)),
            (_, Grapheme::Quot(_)) => (Next, ScanResult::Ok(Append)),
            (Next, Grapheme::Operator(Operator('\\'))) => (Escaped, ScanResult::Ok(Append)),
            (_, Grapheme::Operator(_)) => (Next, ScanResult::Ok(Append)),
            (_, Grapheme::Delimiter(_)) => (Next, ScanResult::Ok(Append)),
            (_, Grapheme::Space(_)) => (Next, ScanResult::Ok(Append)),
            (Escaped, Grapheme::Newline(_)) => (Next, ScanResult::Ok(Append)),
            (_, Grapheme::Newline(_)) => (Next, ScanResult::Err(Terminate)),
            (_, Grapheme::Unknown(_)) => (Next, ScanResult::Ok(Append)),
            (_, Grapheme::Eof) => (Next, ScanResult::Err(Terminate)),
        });

        let end_pos = self.current_position();

        map_tok(res, |str| {
            TokenKind::ByteString(ByteString::from_quot(quot, str, end_pos))
        })
    }

    fn scan_operator(&mut self) -> TokenKind {
        let res = self.scan(|c| match c {
            Grapheme::Alpha(_) => ScanResult::Ok(Terminate),
            Grapheme::Digit(_) => ScanResult::Ok(Terminate),
            Grapheme::Quot(_) => ScanResult::Err(Terminate), // `<='` seems like a weird thing to support...
            Grapheme::Operator(_) => ScanResult::Ok(Append),
            Grapheme::Delimiter(_) => ScanResult::Ok(Terminate),
            Grapheme::Space(_) => ScanResult::Ok(Terminate),
            Grapheme::Newline(_) => ScanResult::Ok(Terminate),
            Grapheme::Unknown(_) => ScanResult::Err(Terminate),
            Grapheme::Eof => ScanResult::Ok(Terminate),
        });

        map_tok(res, |str| TokenKind::Operator(str))
    }
}

impl Iterator for Lexer {
    type Item = TokenId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pos = self.current_position();

            let kind = match self.cursor.get() {
                Grapheme::Unknown(_) => self.scan_unknown(),
                Grapheme::Alpha(_) => self.scan_identifier(),
                Grapheme::Digit(_) => self.scan_numeral(),
                Grapheme::Quot(q) => self.scan_bytestring(q),
                Grapheme::Operator(_) => self.scan_operator(),
                Grapheme::Delimiter(delim) => {
                    self.advance_cursor();
                    TokenKind::Delimiter(delim)
                }
                Grapheme::Space(_) => {
                    self.advance_cursor();
                    continue;
                }
                Grapheme::Newline(_) => {
                    self.advance_cursor();
                    continue;
                }
                Grapheme::Eof => return None,
            };

            break Some(self.tokens.push(Token::new(kind, pos)));
        }
    }
}

impl Index<TokenId> for Lexer {
    type Output = Token;

    fn index(&self, index: TokenId) -> &Self::Output {
        &self.tokens[index]
    }
}

impl Into<TokenVec> for Lexer {
    fn into(self) -> TokenVec {
        self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syn::cursor::Gate;

    #[test]
    fn scan_unknown() {
        let mut lexer = Lexer::new("@@@");

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok].kind(), &TokenKind::Unknown("@@@".into()));
        assert_eq!(lexer[tok].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok].end(), Position::new(1, 3));

        assert!(lexer.next().is_none());
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

            let tok = lexer.next().unwrap();
            assert_eq!(lexer[tok].kind(), &TokenKind::Kw(kw));
            assert_eq!(lexer[tok].begin(), Position::new(1, 0));
            assert_eq!(lexer[tok].end(), Position::new(1, str.len()));

            assert!(lexer.next().is_none());
        }
    }

    #[test]
    fn scan_ident() {
        let mut lexer = Lexer::new("abc");

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok].kind(), &TokenKind::Ident("abc".into()));
        assert_eq!(lexer[tok].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok].end(), Position::new(1, 3));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn scan_ident_with_numerals() {
        let mut lexer = Lexer::new("abc12");

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok].kind(), &TokenKind::Ident("abc12".into()));
        assert_eq!(lexer[tok].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok].end(), Position::new(1, 5));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn scan_strings() {
        let mut lexer = Lexer::new(r#""abc" "def\"""#);

        let tok1 = lexer.next().unwrap();
        let str1 = ByteString::from_quot(Quotation::Double, r#""abc""#, Position::new(1, 5));
        assert_eq!(lexer[tok1].kind(), &TokenKind::ByteString(str1));
        assert_eq!(lexer[tok1].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok1].end(), Position::new(1, 5));

        let tok2 = lexer.next().unwrap();
        let str2 = ByteString::from_quot(Quotation::Double, r#""def\"""#, Position::new(1, 13));
        assert_eq!(lexer[tok2].kind(), &TokenKind::ByteString(str2));
        assert_eq!(lexer[tok2].begin(), Position::new(1, 6));
        assert_eq!(lexer[tok2].end(), Position::new(1, 13));
    }

    #[test]
    fn scan_empty_string() {
        let mut lexer = Lexer::new(r#""""#);
        let tok = lexer.next().unwrap();
        let str = ByteString::new_bytestring(r#""""#, lexer[tok].end());
        assert_eq!(lexer[tok].kind(), &TokenKind::ByteString(str))
    }

    #[test]
    fn scan_erroneous_strings() {
        let mut lexer = Lexer::new(r#""abc"#);

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok].kind(), &TokenKind::Unknown(r#""abc"#.into()));
        assert_eq!(lexer[tok].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok].end(), Position::new(1, 4));
    }

    #[test]
    fn identifiers_dont_start_with_digits() {
        let mut lexer = Lexer::new("1abc");

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok].kind(), &TokenKind::Unknown("1abc".into()));
        assert_eq!(lexer[tok].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok].end(), Position::new(1, 4));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn multiple_tokens() {
        let mut lexer = Lexer::new("abc 123");

        let tok1 = lexer.next().unwrap();
        assert_eq!(lexer[tok1].kind(), &TokenKind::Ident("abc".into()));
        assert_eq!(lexer[tok1].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok1].end(), Position::new(1, 3));

        let tok2 = lexer.next().unwrap();
        assert_eq!(lexer[tok2].kind(), &TokenKind::Numeral("123".into()));
        assert_eq!(lexer[tok2].begin(), Position::new(1, 4));
        assert_eq!(lexer[tok2].end(), Position::new(1, 7));
    }

    #[test]
    fn scan_operators() {
        let mut lexer = Lexer::new(". .. .| ~ && ~()");

        let tok1 = lexer.next().unwrap();
        assert_eq!(lexer[tok1].kind(), &TokenKind::Operator(".".into()));
        assert_eq!(lexer[tok1].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok1].end(), Position::new(1, 1));

        let tok2 = lexer.next().unwrap();
        assert_eq!(lexer[tok2].kind(), &TokenKind::Operator("..".into()));
        assert_eq!(lexer[tok2].begin(), Position::new(1, 2));
        assert_eq!(lexer[tok2].end(), Position::new(1, 4));

        let tok3 = lexer.next().unwrap();
        assert_eq!(lexer[tok3].kind(), &TokenKind::Operator(".|".into()));
        assert_eq!(lexer[tok3].begin(), Position::new(1, 5));
        assert_eq!(lexer[tok3].end(), Position::new(1, 7));

        let tok4 = lexer.next().unwrap();
        assert_eq!(lexer[tok4].kind(), &TokenKind::Operator("~".into()));
        assert_eq!(lexer[tok4].begin(), Position::new(1, 8));
        assert_eq!(lexer[tok4].end(), Position::new(1, 9));

        let tok5 = lexer.next().unwrap();
        assert_eq!(lexer[tok5].kind(), &TokenKind::Operator("&&".into()));
        assert_eq!(lexer[tok5].begin(), Position::new(1, 10));
        assert_eq!(lexer[tok5].end(), Position::new(1, 12));

        let tok6 = lexer.next().unwrap();
        assert_eq!(lexer[tok6].kind(), &TokenKind::Operator("~".into()));
        assert_eq!(lexer[tok6].begin(), Position::new(1, 13));
        assert_eq!(lexer[tok6].end(), Position::new(1, 14));

        let tok7 = lexer.next().unwrap();
        assert_eq!(
            lexer[tok7].kind(),
            &TokenKind::Delimiter(Delimiter::Paren(Gate::Opened))
        );
        assert_eq!(lexer[tok7].begin(), Position::new(1, 14));
        assert_eq!(lexer[tok7].end(), Position::new(1, 15));

        let tok8 = lexer.next().unwrap();
        assert_eq!(
            lexer[tok8].kind(),
            &TokenKind::Delimiter(Delimiter::Paren(Gate::Closed))
        );
        assert_eq!(lexer[tok8].begin(), Position::new(1, 15));
        assert_eq!(lexer[tok8].end(), Position::new(1, 16));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn scan_with_newlines() {
        let mut lexer = Lexer::new("abc def\nghi jkl");

        let tok1 = lexer.next().unwrap();
        assert_eq!(lexer[tok1].kind(), &TokenKind::Ident("abc".into()));
        assert_eq!(lexer[tok1].begin(), Position::new(1, 0));
        assert_eq!(lexer[tok1].end(), Position::new(1, 3));

        let tok2 = lexer.next().unwrap();
        assert_eq!(lexer[tok2].kind(), &TokenKind::Ident("def".into()));
        assert_eq!(lexer[tok2].begin(), Position::new(1, 4));
        assert_eq!(lexer[tok2].end(), Position::new(1, 7));

        let tok3 = lexer.next().unwrap();
        assert_eq!(lexer[tok3].kind(), &TokenKind::Ident("ghi".into()));
        assert_eq!(lexer[tok3].begin(), Position::new(2, 0));
        assert_eq!(lexer[tok3].end(), Position::new(2, 3));

        let tok4 = lexer.next().unwrap();
        assert_eq!(lexer[tok4].kind(), &TokenKind::Ident("jkl".into()));
        assert_eq!(lexer[tok4].begin(), Position::new(2, 4));
        assert_eq!(lexer[tok4].end(), Position::new(2, 7));

        assert!(lexer.next().is_none());
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
