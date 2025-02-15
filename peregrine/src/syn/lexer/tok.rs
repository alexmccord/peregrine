use crate::idx;

use crate::ast::Position;
use crate::syn::cursor::{Gate, Quotation};

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
    ByteString(String),
    ByteChar(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Parity {
    Opened,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Unknown(String),
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

idx::newindex!(pub TokenId);

pub type TokenVec = idx::IndexedVec<TokenId, Token>;

impl TokenVec {
    pub fn get_pos(&self, id: TokenId) -> (Position, Position) {
        let begin = self[id].pos;
        let end = self.get(TokenVec::next(id)).map_or(begin, |tok| tok.pos);
        (begin, end)
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

impl ByteString {
    pub fn new_bytestring(str: impl Into<String>) -> ByteString {
        ByteString::ByteString(str.into())
    }

    pub fn new_bytechar(str: impl Into<String>) -> ByteString {
        ByteString::ByteChar(str.into())
    }

    pub fn from_quot(quot: Quotation, str: impl Into<String>) -> ByteString {
        match quot {
            Quotation::Single => ByteString::new_bytechar(str),
            Quotation::Double => ByteString::new_bytestring(str),
        }
    }

    /// Returns the length of the string including the quotations.
    pub fn len(&self) -> usize {
        match self {
            ByteString::ByteString(str) => str.len(),
            ByteString::ByteChar(str) => str.len(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    kind: TokenKind,
    pos: Position,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, pos: Position) -> Token {
        Token { kind, pos }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl Parity {
    pub fn get_dual(self) -> Parity {
        match self {
            Parity::Opened => Parity::Closed,
            Parity::Closed => Parity::Opened,
        }
    }
}

impl From<Gate> for Parity {
    fn from(value: Gate) -> Self {
        match value {
            Gate::Open => Parity::Opened,
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
