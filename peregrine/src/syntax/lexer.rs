use crate::syntax::cursor::{Alpha, Cursor, Delimiter, Digit, Gate, Grapheme, Operator, Space};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Module,
    Import,
    Struct,
    Data,
    Let,
    Do,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Unknown(String),
    Kw(Keyword),
    Ident(String),
    Numeral(String),
    Operator(String),
    Delimiter(Delimiter),
    Indent,
    Dedent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    fn new(kind: TokenKind) -> Token {
        Token { kind }
    }

    fn unknown(str: String) -> Token {
        Token::new(TokenKind::Unknown(str))
    }

    fn kw(kw: Keyword) -> Token {
        Token::new(TokenKind::Kw(kw))
    }

    fn ident(ident: String) -> Token {
        Token::new(TokenKind::Ident(ident))
    }

    fn numeral(num: String) -> Token {
        Token::new(TokenKind::Numeral(num))
    }

    fn operator(op: String) -> Token {
        Token::new(TokenKind::Operator(op))
    }

    fn delimiter(delim: Delimiter) -> Token {
        Token::new(TokenKind::Delimiter(delim))
    }

    fn indent() -> Token {
        Token::new(TokenKind::Indent)
    }

    fn dedent() -> Token {
        Token::new(TokenKind::Dedent)
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

    pub fn get_op(&self) -> Option<&String> {
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
    Append(char),
    Terminate,
    Error,
}

pub struct Lexer {
    cursor: Cursor,
    lookahead: Option<Token>,
    indent: Vec<Space>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
            lookahead: None,
            indent: Vec::default(),
        }
    }

    pub fn lookahead(&mut self) -> Option<&Token> {
        if self.lookahead.is_none() {
            self.lookahead = self.next();
        }

        self.lookahead.as_ref()
    }

    fn scan(&mut self, char: char, f: fn(Grapheme) -> ScanCommand) -> Result<String, Token> {
        let mut acc = char.to_string();

        loop {
            let current = self.cursor.get();

            match f(current) {
                ScanCommand::Append(c) => acc.push(c),
                ScanCommand::Terminate => return Ok(acc),
                ScanCommand::Error => return Err(Token::unknown(acc)),
            }

            self.cursor.next();
        }
    }

    fn scan_unknown(&mut self, c: char) -> Token {
        let res = self.scan(c, |c| match c {
            Grapheme::Unknown(c) => ScanCommand::Append(c),
            _ => ScanCommand::Terminate,
        });

        match res {
            Ok(str) => Token::unknown(str),
            Err(tok) => tok,
        }
    }

    fn scan_identifier(&mut self, Alpha(alpha): Alpha) -> Token {
        let res = self.scan(alpha, |c| match c {
            Grapheme::Alpha(Alpha(alpha)) => ScanCommand::Append(alpha),
            Grapheme::Digit(Digit(digit)) => ScanCommand::Append(digit),
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

    fn map_to_keyword(ident: &String) -> Option<Keyword> {
        match ident.as_str() {
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "struct" => Some(Keyword::Struct),
            "data" => Some(Keyword::Data),
            "let" => Some(Keyword::Let),
            "do" => Some(Keyword::Do),
            _ => None,
        }
    }

    fn scan_numeral(&mut self, Digit(digit): Digit) -> Token {
        let res = self.scan(digit, |c| match c {
            Grapheme::Alpha(_) => ScanCommand::Error,
            Grapheme::Digit(Digit(digit)) => ScanCommand::Append(digit),
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

    fn scan_operator(&mut self, Operator(op): Operator) -> Token {
        let res = self.scan(op, |c| match c {
            Grapheme::Alpha(_) => ScanCommand::Terminate,
            Grapheme::Digit(_) => ScanCommand::Terminate,
            Grapheme::Operator(Operator(c)) => ScanCommand::Append(c),
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

    fn scan_delimiter(&mut self, delim: Delimiter) -> Token {
        Token::delimiter(delim)
    }

    fn scan_indent(&mut self) -> Option<Token> {
        let mut stack = Vec::new();

        loop {
            match self.cursor.get() {
                Grapheme::Alpha(Alpha(_)) => break,
                Grapheme::Digit(Digit(_)) => break,
                Grapheme::Operator(Operator(_)) => break,
                Grapheme::Delimiter(_) => break,
                Grapheme::Space(space) => stack.push(space),
                Grapheme::Newline(_) => stack.clear(),
                Grapheme::Unknown(_) => break,
                Grapheme::Eof => break,
            }

            self.cursor.next();
        }

        for (l, r) in self.indent.iter().zip(stack.iter()) {
            if l != r {
                let mut acc = String::new();
                for Space(c) in stack {
                    acc.push(c);
                }

                return Some(Token::unknown(acc));
            }
        }

        let tok = match stack.len().cmp(&self.indent.len()) {
            std::cmp::Ordering::Less => Some(Token::dedent()),
            std::cmp::Ordering::Equal => None,
            std::cmp::Ordering::Greater => Some(Token::indent()),
        };

        self.indent = stack;

        tok
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.lookahead.take() {
            return Some(tok);
        }

        let Some(c) = self.cursor.next() else {
            return None;
        };

        match c {
            Grapheme::Unknown(c) => Some(self.scan_unknown(c)),
            Grapheme::Alpha(alpha) => Some(self.scan_identifier(alpha)),
            Grapheme::Digit(digit) => Some(self.scan_numeral(digit)),
            Grapheme::Operator(op) => Some(self.scan_operator(op)),
            Grapheme::Delimiter(delim) => Some(self.scan_delimiter(delim)),
            Grapheme::Space(_) => self.next(),
            Grapheme::Newline(_) => self.scan_indent().or_else(|| self.next()),
            Grapheme::Eof => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Delimiter, Gate, Keyword, Lexer, Token};

    #[test]
    fn scan_unknown() {
        let mut lexer = Lexer::new("@@@".to_string());
        assert_eq!(lexer.next(), Some(Token::unknown("@@@".to_string())));
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

        for (str, kw) in kws {
            let mut lexer = Lexer::new(str.to_string());
            assert_eq!(lexer.next(), Some(Token::kw(kw)));
        }
    }

    #[test]
    fn scan_ident() {
        let mut lexer = Lexer::new("abc".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("abc".to_string())));
    }

    #[test]
    fn scan_ident_with_numerals() {
        let mut lexer = Lexer::new("abc12".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("abc12".to_string())));
    }

    #[test]
    fn identifiers_dont_start_with_digits() {
        let mut lexer = Lexer::new("1abc".to_string());
        assert_eq!(lexer.next(), Some(Token::unknown("1".to_string())));
    }

    #[test]
    fn multiple_tokens() {
        let mut lexer = Lexer::new("abc 123".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("abc".to_string())));
        assert_eq!(lexer.next(), Some(Token::numeral("123".to_string())));
    }

    #[test]
    fn scan_operators() {
        let mut lexer = Lexer::new(". .. .| ~ && ~()".to_string());
        assert_eq!(lexer.next(), Some(Token::operator(".".to_string())));
        assert_eq!(lexer.next(), Some(Token::operator("..".to_string())));
        assert_eq!(lexer.next(), Some(Token::operator(".|".to_string())));
        assert_eq!(lexer.next(), Some(Token::operator("~".to_string())));
        assert_eq!(lexer.next(), Some(Token::operator("&&".to_string())));
        assert_eq!(lexer.next(), Some(Token::operator("~".to_string())));
        assert_eq!(
            lexer.next(),
            Some(Token::delimiter(Delimiter::Paren(Gate::Opened)))
        );
        assert_eq!(
            lexer.next(),
            Some(Token::delimiter(Delimiter::Paren(Gate::Closed)))
        );
    }

    #[test]
    fn scan_indentation() {
        let mut lexer = Lexer::new("\na\n b\n  c\n d\ne".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("a".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("b".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("c".to_string())));
        assert_eq!(lexer.next(), Some(Token::dedent()));
        assert_eq!(lexer.next(), Some(Token::ident("d".to_string())));
        assert_eq!(lexer.next(), Some(Token::dedent()));
        assert_eq!(lexer.next(), Some(Token::ident("e".to_string())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn indent_token_are_emitted_only_if_indentation_changes() {
        let mut lexer = Lexer::new("a\n  b\n  c\n  \td\n  \t  e".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("a".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("b".to_string())));
        assert_eq!(lexer.next(), Some(Token::ident("c".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("d".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("e".to_string())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn no_mixed_indentation() {
        let mut lexer = Lexer::new("a\n  b\n\t\tc".to_string());
        assert_eq!(lexer.next(), Some(Token::ident("a".to_string())));
        assert_eq!(lexer.next(), Some(Token::indent()));
        assert_eq!(lexer.next(), Some(Token::ident("b".to_string())));
        assert_eq!(lexer.next(), Some(Token::unknown("\t\t".to_string())));
        assert_eq!(lexer.next(), Some(Token::ident("c".to_string())));
        assert_eq!(lexer.next(), None);
    }
}
