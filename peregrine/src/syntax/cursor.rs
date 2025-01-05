#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Alpha(pub char);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Digit(pub char);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Operator(pub char);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Gate {
    Opened,
    Closed,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Delimiter {
    Paren(Gate),
    Brace(Gate),
    Bracket(Gate),
}

impl Delimiter {
    pub fn as_byte(&self) -> u8 {
        match self {
            Delimiter::Paren(Gate::Opened) => b'(',
            Delimiter::Paren(Gate::Closed) => b')',
            Delimiter::Brace(Gate::Opened) => b'{',
            Delimiter::Brace(Gate::Closed) => b'}',
            Delimiter::Bracket(Gate::Opened) => b'[',
            Delimiter::Bracket(Gate::Closed) => b']',
        }
    }

    pub fn as_char(&self) -> char {
        self.as_byte().into()
    }

    pub fn from_byte(c: u8) -> Option<Delimiter> {
        match c {
            b'(' => Some(Delimiter::Paren(Gate::Opened)),
            b')' => Some(Delimiter::Paren(Gate::Closed)),
            b'{' => Some(Delimiter::Brace(Gate::Opened)),
            b'}' => Some(Delimiter::Brace(Gate::Closed)),
            b'[' => Some(Delimiter::Bracket(Gate::Opened)),
            b']' => Some(Delimiter::Bracket(Gate::Closed)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Space(pub char);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Newline {
    LF,   // \n
    CRLF, // \r\n
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Grapheme {
    Alpha(Alpha),
    Digit(Digit),
    Operator(Operator),
    Delimiter(Delimiter),
    Space(Space),
    Newline(Newline),
    Unknown(char),
    Eof,
}

impl Grapheme {
    fn len(&self) -> usize {
        match self {
            Grapheme::Alpha(_) => 1,
            Grapheme::Digit(_) => 1,
            Grapheme::Operator(_) => 1,
            Grapheme::Delimiter(_) => 1,
            Grapheme::Space(_) => 1,
            Grapheme::Newline(Newline::LF) => 1,
            Grapheme::Newline(Newline::CRLF) => 2,
            Grapheme::Unknown(_) => 1,
            Grapheme::Eof => 0,
        }
    }
}

pub struct Cursor<'input> {
    input: &'input str,
    offset: usize,
}

impl<'input> Cursor<'input> {
    pub fn new(input: &'input str) -> Cursor<'input> {
        Cursor { input, offset: 0 }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn slice(&'input self, i: usize, j: usize) -> &'input str {
        &self.input[i..j]
    }

    pub fn seek_chars(&self) -> &[u8] {
        if self.offset >= self.input.len() {
            &[]
        } else {
            self.input.as_bytes().split_at(self.offset).1
        }
    }

    pub fn get(&self) -> Grapheme {
        match self.seek_chars() {
            [] => Grapheme::Eof,
            [b'\r', b'\n', ..] => Grapheme::Newline(Newline::CRLF),
            [b'\n', ..] => Grapheme::Newline(Newline::LF),
            [head, ..] => match head {
                &c if Cursor::is_alpha(c) => Grapheme::Alpha(Alpha(c.into())),
                &c if Cursor::is_digit(c) => Grapheme::Digit(Digit(c.into())),
                &c if Cursor::is_operator(c) => Grapheme::Operator(Operator(c.into())),
                &c if Delimiter::from_byte(c).is_some() => {
                    Grapheme::Delimiter(Delimiter::from_byte(c).unwrap())
                }
                &c if Cursor::is_space(c) => Grapheme::Space(Space(c.into())),
                &c => Grapheme::Unknown(c.into()),
            },
        }
    }

    fn is_alpha(c: u8) -> bool {
        matches!(c, b'a'..=b'z' | b'A'..=b'Z')
    }

    fn is_digit(c: u8) -> bool {
        matches!(c, b'0'..=b'9')
    }

    fn is_operator(c: u8) -> bool {
        false
            || c == b'~'
            || c == b'&'
            || c == b'|'
            || c == b'^'
            || c == b'%'
            || c == b'*'
            || c == b'-'
            || c == b'+'
            || c == b'/'
            || c == b'\\'
            || c == b'='
            || c == b':'
            || c == b';'
            || c == b','
            || c == b'!'
            || c == b'?'
            || c == b'.'
            || c == b'<'
            || c == b'>'
    }

    fn is_space(c: u8) -> bool {
        matches!(c, b' ' | b'\t')
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = Grapheme;

    fn next(&mut self) -> Option<Self::Item> {
        let grapheme = self.get();
        self.offset += grapheme.len();

        match grapheme {
            Grapheme::Eof => None,
            c => Some(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Alpha, Cursor, Digit, Grapheme, Newline};

    #[test]
    fn get_alpha_lowercase() {
        for c in 'a'..='z' {
            let str = c.to_string();
            let cursor = Cursor::new(&str);
            assert_eq!(cursor.get(), Grapheme::Alpha(Alpha(c)));
        }
    }

    #[test]
    fn get_alpha_uppercase() {
        for c in 'A'..='Z' {
            let str = c.to_string();
            let cursor = Cursor::new(&str);
            assert_eq!(cursor.get(), Grapheme::Alpha(Alpha(c)));
        }
    }

    #[test]
    fn get_digit() {
        for c in '0'..='9' {
            let str = c.to_string();
            let cursor = Cursor::new(&str);
            assert_eq!(cursor.get(), Grapheme::Digit(Digit(c)));
        }
    }

    #[test]
    fn get_eof() {
        let cursor = Cursor::new("");
        assert_eq!(cursor.get(), Grapheme::Eof);
    }

    #[test]
    fn get_all() {
        let mut cursor = Cursor::new("abAB12");
        assert_eq!(cursor.next(), Some(Grapheme::Alpha(Alpha('a'))));
        assert_eq!(cursor.next(), Some(Grapheme::Alpha(Alpha('b'))));
        assert_eq!(cursor.next(), Some(Grapheme::Alpha(Alpha('A'))));
        assert_eq!(cursor.next(), Some(Grapheme::Alpha(Alpha('B'))));
        assert_eq!(cursor.next(), Some(Grapheme::Digit(Digit('1'))));
        assert_eq!(cursor.next(), Some(Grapheme::Digit(Digit('2'))));
        assert_eq!(cursor.next(), None);
        assert_eq!(cursor.next(), None);
    }

    #[test]
    fn get_newlines() {
        let mut cursor = Cursor::new("\r\n\n");
        assert_eq!(cursor.next(), Some(Grapheme::Newline(Newline::CRLF)));
        assert_eq!(cursor.next(), Some(Grapheme::Newline(Newline::LF)));
        assert_eq!(cursor.next(), None);
    }
}
