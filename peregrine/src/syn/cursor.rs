#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Alpha(pub char);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Digit {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
}

impl Digit {
    pub fn to_char(self) -> char {
        self.as_byte().into()
    }

    pub fn as_byte(self) -> u8 {
        match self {
            Digit::Zero => b'0',
            Digit::One => b'1',
            Digit::Two => b'2',
            Digit::Three => b'3',
            Digit::Four => b'4',
            Digit::Five => b'5',
            Digit::Six => b'6',
            Digit::Seven => b'7',
            Digit::Eight => b'8',
            Digit::Nine => b'9',
        }
    }

    pub fn from_byte(c: u8) -> Option<Digit> {
        match c {
            b'0' => Some(Digit::Zero),
            b'1' => Some(Digit::One),
            b'2' => Some(Digit::Two),
            b'3' => Some(Digit::Three),
            b'4' => Some(Digit::Four),
            b'5' => Some(Digit::Five),
            b'6' => Some(Digit::Six),
            b'7' => Some(Digit::Seven),
            b'8' => Some(Digit::Eight),
            b'9' => Some(Digit::Nine),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Quotation {
    Single, // 'a'
    Double, // "a"
}

impl Quotation {
    pub fn to_char(self) -> char {
        self.as_byte().into()
    }

    pub(crate) fn as_byte(self) -> u8 {
        match self {
            Quotation::Single => b'\'',
            Quotation::Double => b'\"',
        }
    }

    pub(crate) fn from_byte(c: u8) -> Option<Quotation> {
        match c {
            b'\'' => Some(Quotation::Single),
            b'\"' => Some(Quotation::Double),
            _ => None,
        }
    }
}

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
    Semicolon,
    Comma,
}

impl Delimiter {
    pub fn to_char(self) -> char {
        self.as_byte().into()
    }

    pub fn len(self) -> usize {
        match self {
            Delimiter::Paren(_) => 1,
            Delimiter::Brace(_) => 1,
            Delimiter::Bracket(_) => 1,
            Delimiter::Semicolon => 1,
            Delimiter::Comma => 1,
        }
    }

    pub(crate) fn as_byte(self) -> u8 {
        match self {
            Delimiter::Paren(Gate::Opened) => b'(',
            Delimiter::Paren(Gate::Closed) => b')',
            Delimiter::Brace(Gate::Opened) => b'{',
            Delimiter::Brace(Gate::Closed) => b'}',
            Delimiter::Bracket(Gate::Opened) => b'[',
            Delimiter::Bracket(Gate::Closed) => b']',
            Delimiter::Semicolon => b';',
            Delimiter::Comma => b',',
        }
    }

    pub(crate) fn from_byte(c: u8) -> Option<Delimiter> {
        match c {
            b'(' => Some(Delimiter::Paren(Gate::Opened)),
            b')' => Some(Delimiter::Paren(Gate::Closed)),
            b'{' => Some(Delimiter::Brace(Gate::Opened)),
            b'}' => Some(Delimiter::Brace(Gate::Closed)),
            b'[' => Some(Delimiter::Bracket(Gate::Opened)),
            b']' => Some(Delimiter::Bracket(Gate::Closed)),
            b';' => Some(Delimiter::Semicolon),
            b',' => Some(Delimiter::Comma),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Space {
    Space,
    Tab,
}

impl Space {
    pub fn from_byte(c: u8) -> Option<Space> {
        match c {
            b' ' => Some(Space::Space),
            b'\t' => Some(Space::Tab),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Newline {
    LF,   // \n
    CRLF, // \r\n
}

impl Newline {
    pub fn len(self) -> usize {
        match self {
            Newline::LF => 1,
            Newline::CRLF => 2,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Grapheme {
    Alpha(Alpha),
    Digit(Digit),
    Quot(Quotation),
    Operator(Operator),
    Delimiter(Delimiter),
    Space(Space),
    Newline(Newline),
    Unknown(char),
    Eof,
}

impl Grapheme {
    pub fn len(&self) -> usize {
        match self {
            Grapheme::Alpha(_) => 1,
            Grapheme::Digit(_) => 1,
            Grapheme::Quot(_) => 1,
            Grapheme::Operator(_) => 1,
            Grapheme::Delimiter(_) => 1,
            Grapheme::Space(_) => 1,
            Grapheme::Newline(nl) => nl.len(),
            Grapheme::Unknown(_) => 1,
            Grapheme::Eof => 0,
        }
    }
}

#[derive(Debug)]
pub struct Cursor {
    input: String,
    offset: usize,
}

impl Cursor {
    pub fn new(input: impl Into<String>) -> Cursor {
        Cursor {
            input: input.into(),
            offset: 0,
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn slice(&self, i: usize, j: usize) -> &str {
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
        // The guts of the parser. Smelly as hell, in the sewers it belongs.
        match self.seek_chars() {
            &[] => Grapheme::Eof,
            &[b'\r', b'\n', ..] => Grapheme::Newline(Newline::CRLF),
            &[b'\n', ..] => Grapheme::Newline(Newline::LF),
            &[c @ (b'a'..=b'z' | b'A'..=b'Z'), ..] => Grapheme::Alpha(Alpha(c.into())),
            &[c @ (b'~' | b'&' | b'|' | b'^' | b'%' | b'*' | b'-' | b'+' | b'/' | b'\\' | b'='
            | b':' | b'!' | b'?' | b'.' | b'<' | b'>'), ..] => {
                Grapheme::Operator(Operator(c.into()))
            }
            &[c, ..] => {
                if let Some(d) = Digit::from_byte(c) {
                    Grapheme::Digit(d)
                } else if let Some(d) = Delimiter::from_byte(c) {
                    Grapheme::Delimiter(d)
                } else if let Some(q) = Quotation::from_byte(c) {
                    Grapheme::Quot(q)
                } else if let Some(s) = Space::from_byte(c) {
                    Grapheme::Space(s)
                } else {
                    Grapheme::Unknown(c.into())
                }
            }
        }
    }
}

impl Iterator for Cursor {
    type Item = Grapheme;

    fn next(&mut self) -> Option<Self::Item> {
        match self.get() {
            Grapheme::Eof => None,
            c => {
                self.offset += c.len();
                Some(c)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_alpha_lowercase() {
        for c in 'a'..='z' {
            let str = c;
            let cursor = Cursor::new(str);
            assert_eq!(cursor.get(), Grapheme::Alpha(Alpha(c)));
        }
    }

    #[test]
    fn get_alpha_uppercase() {
        for c in 'A'..='Z' {
            let str = c;
            let cursor = Cursor::new(str);
            assert_eq!(cursor.get(), Grapheme::Alpha(Alpha(c)));
        }
    }

    #[test]
    fn get_digit() {
        let digits = vec![
            ('0', Digit::Zero),
            ('1', Digit::One),
            ('2', Digit::Two),
            ('3', Digit::Three),
            ('4', Digit::Four),
            ('5', Digit::Five),
            ('6', Digit::Six),
            ('7', Digit::Seven),
            ('8', Digit::Eight),
            ('9', Digit::Nine),
        ];

        for (c, d) in digits {
            let str = c;
            let cursor = Cursor::new(str);
            assert_eq!(cursor.get(), Grapheme::Digit(d));
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
        assert_eq!(cursor.next(), Some(Grapheme::Digit(Digit::One)));
        assert_eq!(cursor.next(), Some(Grapheme::Digit(Digit::Two)));
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
