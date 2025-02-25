use crate::idx;

use crate::ast::Position;
use crate::syn::cursor::{Gate, Quotation};
use crate::syn::offside::{Measured, OffsideTape};

idx::newindex!(pub TokenId);

pub type TokenVec = idx::IndexedVec<TokenId, Token>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    kind: TokenKind,
    pos: Position,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Unknown(Unknown),
    Kw(Keyword),
    Ident(String),
    Numeral(String),
    ByteString(ByteString),
    Operator(String),
    Group(Group),
    Ws(Ws),
    Semicolon,
    Comma,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenBuf {
    str: String,
    is_whitespace_insignificant: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unknown(TokenBuf);

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
    Record,
    Data,
    Deriving,

    // Trait stuff
    Trait,
    Impl,

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
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ByteString {
    ByteString(TokenBuf),
    ByteChar(TokenBuf),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Parity {
    Open,
    Closed,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Group {
    Paren(Parity),
    Brace(Parity),
    Bracket(Parity),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Ws {
    Space { count: usize },
    Tab { count: usize },
    Newline { count: usize },
}

pub trait TokenDetail {
    fn to_str(&self) -> &str;

    fn len(&self) -> usize {
        self.to_str().len()
    }
}

impl TokenVec {
    pub fn get_pos(&self, id: TokenId) -> (Position, Position) {
        let begin = self[id].pos;
        let end = self.get(TokenVec::next_id(id)).map_or(begin, |tok| tok.pos);
        (begin, end)
    }
}

impl Token {
    pub(crate) fn new(kind: TokenKind, pos: Position) -> Token {
        Token { kind, pos }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl TokenBuf {
    pub fn new(str: String, is_whitespace_insignificant: bool) -> TokenBuf {
        TokenBuf {
            str,
            is_whitespace_insignificant,
        }
    }
}

impl Unknown {
    pub fn new(str: String, is_whitespace_insignificant: bool) -> Unknown {
        Unknown(TokenBuf::new(str, is_whitespace_insignificant))
    }

    pub fn is_whitespace_insignificant(&self) -> bool {
        self.0.is_whitespace_insignificant
    }
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
            "record" => Some(Keyword::Record),
            "data" => Some(Keyword::Data),
            "trait" => Some(Keyword::Trait),
            "impl" => Some(Keyword::Impl),
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
}

impl ByteString {
    pub fn new_bytestring(str: impl Into<String>, is_whitespace_insignificant: bool) -> ByteString {
        ByteString::ByteString(TokenBuf::new(str.into(), is_whitespace_insignificant))
    }

    pub fn new_bytechar(str: impl Into<String>, is_whitespace_insignificant: bool) -> ByteString {
        ByteString::ByteChar(TokenBuf::new(str.into(), is_whitespace_insignificant))
    }

    pub fn from_quot(
        quot: Quotation,
        str: impl Into<String>,
        is_whitespace_insignificant: bool,
    ) -> ByteString {
        match quot {
            Quotation::Single => ByteString::new_bytechar(str, is_whitespace_insignificant),
            Quotation::Double => ByteString::new_bytestring(str, is_whitespace_insignificant),
        }
    }

    pub fn is_whitespace_insignificant(&self) -> bool {
        match self {
            ByteString::ByteString(token_buf) => token_buf.is_whitespace_insignificant,
            ByteString::ByteChar(token_buf) => token_buf.is_whitespace_insignificant,
        }
    }
}

impl Parity {
    pub fn get_dual(self) -> Parity {
        match self {
            Parity::Open => Parity::Closed,
            Parity::Closed => Parity::Open,
        }
    }
}

impl From<Gate> for Parity {
    fn from(value: Gate) -> Self {
        match value {
            Gate::Open => Parity::Open,
            Gate::Closed => Parity::Closed,
        }
    }
}

impl Group {
    pub fn get_dual(self) -> Group {
        match self {
            Group::Paren(p) => Group::Paren(p.get_dual()),
            Group::Brace(p) => Group::Brace(p.get_dual()),
            Group::Bracket(p) => Group::Bracket(p.get_dual()),
        }
    }
}

impl Ws {
    pub fn is_space(&self) -> bool {
        matches!(self, Ws::Space { .. })
    }

    pub fn is_tab(&self) -> bool {
        matches!(self, Ws::Tab { .. })
    }

    pub fn is_newline(&self) -> bool {
        matches!(self, Ws::Newline { .. })
    }
}

impl TokenDetail for TokenBuf {
    fn to_str(&self) -> &str {
        &self.str
    }
}

impl TokenDetail for Unknown {
    fn to_str(&self) -> &str {
        self.0.to_str()
    }
}

impl TokenDetail for Keyword {
    fn to_str(&self) -> &str {
        match self {
            Keyword::Module => "module",
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Public => "public",
            Keyword::Open => "open",
            Keyword::Hiding => "hiding",
            Keyword::Renaming => "renaming",
            Keyword::Record => "record",
            Keyword::Data => "data",
            Keyword::Deriving => "deriving",
            Keyword::Trait => "trait",
            Keyword::Impl => "impl",
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
}

impl TokenDetail for ByteString {
    fn to_str(&self) -> &str {
        match self {
            ByteString::ByteString(token_buf) => token_buf.to_str(),
            ByteString::ByteChar(token_buf) => token_buf.to_str(),
        }
    }
}

impl TokenDetail for Group {
    fn to_str(&self) -> &str {
        match self {
            Group::Paren(Parity::Open) => "(",
            Group::Paren(Parity::Closed) => ")",
            Group::Brace(Parity::Open) => "{",
            Group::Brace(Parity::Closed) => "}",
            Group::Bracket(Parity::Open) => "[",
            Group::Bracket(Parity::Closed) => "]",
        }
    }
}

impl OffsideTape for Token {
    fn measure(&self) -> Option<Measured> {
        self.kind().measure()
    }
}

impl OffsideTape for TokenKind {
    fn measure(&self) -> Option<Measured> {
        match self {
            TokenKind::Unknown(u) => u.measure(),
            TokenKind::Kw(kw) => kw.measure(),
            TokenKind::Ident(ident) => Some(Measured::Monospace(ident.len() as u16)),
            TokenKind::Numeral(num) => Some(Measured::Monospace(num.len() as u16)),
            TokenKind::ByteString(byte_string) => byte_string.measure(),
            TokenKind::Operator(op) => Some(Measured::Monospace(op.len() as u16)),
            TokenKind::Group(g) => g.measure(),
            TokenKind::Ws(ws) => ws.measure(),
            TokenKind::Semicolon => Some(Measured::Monospace(1)),
            TokenKind::Comma => Some(Measured::Monospace(1)),
            TokenKind::Eof => Some(Measured::Retract),
        }
    }
}

impl OffsideTape for TokenBuf {
    fn measure(&self) -> Option<Measured> {
        if self.is_whitespace_insignificant {
            return None;
        }

        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Unknown {
    fn measure(&self) -> Option<Measured> {
        self.0.measure()
    }
}

impl OffsideTape for Keyword {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for ByteString {
    fn measure(&self) -> Option<Measured> {
        match self {
            ByteString::ByteString(token_buf) => token_buf.measure(),
            ByteString::ByteChar(token_buf) => token_buf.measure(),
        }
    }
}

impl OffsideTape for Group {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Ws {
    fn measure(&self) -> Option<Measured> {
        match self {
            Ws::Space { count } => Some(Measured::Monospace(*count as u16)),
            Ws::Tab { count } => Some(Measured::Elastic(*count as u16)),
            Ws::Newline { .. } => Some(Measured::Retract),
        }
    }
}
