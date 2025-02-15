#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ScanUnit {
    Alpha(Alpha),
    Digit(Digit),
    Quot(Quotation),
    Operator(Operator),
    Delimiter(Delimiter),
    Space(Space),
    Newline(Newline),
    Unknown(Unknown),
    Eof,
}

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Quotation {
    Single, // 'a'
    Double, // "a"
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Operator(pub char);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Gate {
    Open,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Space {
    Space,
    Tab,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Newline {
    LF,   // \n
    CR,   // \r
    CRLF, // \r\n
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Unknown(pub char);

impl ScanUnit {
    pub fn byte_len(&self) -> usize {
        match self {
            ScanUnit::Alpha(a) => a.len(),
            ScanUnit::Digit(d) => d.len(),
            ScanUnit::Quot(q) => q.len(),
            ScanUnit::Operator(o) => o.len(),
            ScanUnit::Delimiter(d) => d.len(),
            ScanUnit::Space(s) => s.len(),
            ScanUnit::Newline(nl) => nl.len(),
            ScanUnit::Unknown(u) => u.len(),
            ScanUnit::Eof => 1,
        }
    }

    /// Returns an Option because tabs have no known character length.
    pub fn char_len(&self) -> Option<usize> {
        match self {
            ScanUnit::Alpha(_) => Some(1),
            ScanUnit::Digit(_) => Some(1),
            ScanUnit::Quot(_) => Some(1),
            ScanUnit::Operator(_) => Some(1),
            ScanUnit::Delimiter(_) => Some(1),
            ScanUnit::Space(Space::Space) => Some(1),
            ScanUnit::Space(Space::Tab) => None,
            ScanUnit::Newline(_) => Some(0),
            ScanUnit::Unknown(_) => Some(1),
            ScanUnit::Eof => Some(0),
        }
    }
}

impl Alpha {
    pub fn len(self) -> usize {
        self.0.len_utf8()
    }
}

impl Digit {
    pub fn as_char(self) -> char {
        match self {
            Digit::Zero => '0',
            Digit::One => '1',
            Digit::Two => '2',
            Digit::Three => '3',
            Digit::Four => '4',
            Digit::Five => '5',
            Digit::Six => '6',
            Digit::Seven => '7',
            Digit::Eight => '8',
            Digit::Nine => '9',
        }
    }

    pub fn from_char(c: char) -> Option<Digit> {
        match c {
            '0' => Some(Digit::Zero),
            '1' => Some(Digit::One),
            '2' => Some(Digit::Two),
            '3' => Some(Digit::Three),
            '4' => Some(Digit::Four),
            '5' => Some(Digit::Five),
            '6' => Some(Digit::Six),
            '7' => Some(Digit::Seven),
            '8' => Some(Digit::Eight),
            '9' => Some(Digit::Nine),
            _ => None,
        }
    }

    pub fn len(self) -> usize {
        self.as_char().len_utf8()
    }
}

impl Quotation {
    pub(crate) fn as_char(self) -> char {
        match self {
            Quotation::Single => '\'',
            Quotation::Double => '\"',
        }
    }

    pub(crate) fn from_char(c: char) -> Option<Quotation> {
        match c {
            '\'' => Some(Quotation::Single),
            '\"' => Some(Quotation::Double),
            _ => None,
        }
    }

    pub fn len(self) -> usize {
        self.as_char().len_utf8()
    }
}

impl Operator {
    pub fn len(self) -> usize {
        self.0.len_utf8()
    }
}

impl Delimiter {
    pub(crate) fn as_char(self) -> char {
        match self {
            Delimiter::Paren(Gate::Open) => '(',
            Delimiter::Paren(Gate::Closed) => ')',
            Delimiter::Brace(Gate::Open) => '{',
            Delimiter::Brace(Gate::Closed) => '}',
            Delimiter::Bracket(Gate::Open) => '[',
            Delimiter::Bracket(Gate::Closed) => ']',
            Delimiter::Semicolon => ';',
            Delimiter::Comma => ',',
        }
    }

    pub(crate) fn from_char(c: char) -> Option<Delimiter> {
        match c {
            '(' => Some(Delimiter::Paren(Gate::Open)),
            ')' => Some(Delimiter::Paren(Gate::Closed)),
            '{' => Some(Delimiter::Brace(Gate::Open)),
            '}' => Some(Delimiter::Brace(Gate::Closed)),
            '[' => Some(Delimiter::Bracket(Gate::Open)),
            ']' => Some(Delimiter::Bracket(Gate::Closed)),
            ';' => Some(Delimiter::Semicolon),
            ',' => Some(Delimiter::Comma),
            _ => None,
        }
    }

    pub fn len(self) -> usize {
        self.as_char().len_utf8()
    }
}

impl Space {
    pub fn as_char(self) -> char {
        match self {
            Space::Space => ' ',
            Space::Tab => '\t',
        }
    }

    pub fn from_char(c: char) -> Option<Space> {
        match c {
            ' ' => Some(Space::Space),
            '\t' => Some(Space::Tab),
            _ => None,
        }
    }

    pub fn len(self) -> usize {
        self.as_char().len_utf8()
    }
}

impl Newline {
    pub fn len(self) -> usize {
        match self {
            Newline::LF => 1,
            Newline::CR => 1,
            Newline::CRLF => 2,
        }
    }
}

impl Unknown {
    pub fn len(self) -> usize {
        self.0.len_utf8()
    }

    pub fn is_ascii(self) -> bool {
        self.0.is_ascii()
    }
}
