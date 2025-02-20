use crate::ast::Position;
use crate::syn::cursor::{Cursor, Delimiter, Operator, Quotation, ScanUnit, Space};
use crate::syn::offside::Offside;

pub mod tok;
use tok::*;

use super::offside::Absolute;

mod tests;

#[derive(Debug)]
pub(crate) struct Lexer {
    cursor: Cursor,
    current_pos: Position,
    offside: Offside,
}

#[derive(Debug)]
pub struct TokenStream {
    lexer: Lexer,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Scan {
    Ok(Cmd),
    Err(Cmd),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Cmd {
    Append,
    Terminate,
}

fn map_tok<T, F>(res: Result<T, TokenKind>, f: F) -> TokenKind
where
    F: FnOnce(T) -> TokenKind,
{
    res.map_or_else(|tok| tok, |v| f(v))
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
            current_pos: Position::new(1, 0),
            offside: Offside::new(),
        }
    }

    fn current_position(&self) -> Position {
        self.current_pos
    }

    fn advance_cursor(&mut self) {
        if let Some(su) = self.cursor.next() {
            self.current_pos = match su {
                ScanUnit::Newline(_) => Position::new(self.current_pos.line() + 1, 0),
                _ => self.current_pos.add(0, 1),
            };

            self.offside.push_scan_unit(su);
        }
    }

    fn take_while<F>(&mut self, f: F) -> Result<(usize, usize), TokenKind>
    where
        F: Fn(ScanUnit) -> Scan,
    {
        self.take_while_with((), |(), su| ((), f(su))).1
    }

    fn take_while_with<T, F>(
        &mut self,
        mut state: T,
        f: F,
    ) -> (T, Result<(usize, usize), TokenKind>)
    where
        F: Fn(T, ScanUnit) -> (T, Scan),
    {
        let i = self.cursor.byte_offset();
        let mut ok = true;

        // We call `self.advance_cursor()` here and now without the loop because
        // `self.next()` already checked what ScanUnit we have so we can skip that
        // redundant computation. But we have to do this after we get our `i`,
        // otherwise slicing will be off by one byte at the start.
        self.advance_cursor();

        loop {
            let (s, res) = f(state, self.cursor.get());
            state = s;

            let command = match res {
                Scan::Ok(cmd) => cmd,
                Scan::Err(cmd) => {
                    ok = false;
                    cmd
                }
            };

            let result = match command {
                Cmd::Append => {
                    self.advance_cursor();
                    continue;
                }
                Cmd::Terminate => {
                    let j = self.cursor.byte_offset();

                    if ok {
                        Ok((i, j))
                    } else {
                        Err(TokenKind::Unknown(self.cursor.slice(i, j).to_string()))
                    }
                }
            };

            return (state, result);
        }
    }

    fn scan<F>(&mut self, f: F) -> Result<String, TokenKind>
    where
        F: Fn(ScanUnit) -> Scan,
    {
        self.scan_with((), |(), su| ((), f(su))).1
    }

    fn scan_with<T, F>(&mut self, state: T, f: F) -> (T, Result<String, TokenKind>)
    where
        F: Fn(T, ScanUnit) -> (T, Scan),
    {
        let (state, res) = self.take_while_with(state, f);

        match res {
            Ok((i, j)) => (state, Ok(self.cursor.slice(i, j).to_string())),
            Err(tok) => (state, Err(tok)),
        }
    }

    fn scan_unknown(&mut self) -> TokenKind {
        // We don't really _need_ for scan to return a Result
        // in this case, but... why duplicate logic? /shrug.
        let res = self.scan(|c| match c {
            ScanUnit::Unknown(_) => Scan::Err(Cmd::Append),
            _ => Scan::Err(Cmd::Terminate),
        });

        map_tok(res, |str| TokenKind::Unknown(str))
    }

    fn scan_identifier(&mut self) -> TokenKind {
        let res = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Append), // TODO: enforce it to be postfix?
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unknown(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |str| match Keyword::find(&str) {
            Some(kw) => TokenKind::Kw(kw),
            None => TokenKind::Ident(str),
        })
    }

    fn scan_numeral(&mut self) -> TokenKind {
        let res = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Err(Cmd::Append),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Quot(_) => Scan::Err(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unknown(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
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

        let (_, res) = self.scan_with(Next, |s, su| match (s, su) {
            (Finished, _) => (Finished, Scan::Ok(Cmd::Terminate)),
            (_, ScanUnit::Alpha(_)) => (Next, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Digit(_)) => (Next, Scan::Ok(Cmd::Append)),
            (Next, ScanUnit::Quot(q)) if q == quot => (Finished, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Quot(_)) => (Next, Scan::Ok(Cmd::Append)),
            (Next, ScanUnit::Operator(Operator('\\'))) => (Escaped, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Operator(_)) => (Next, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Delimiter(_)) => (Next, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Space(_)) => (Next, Scan::Ok(Cmd::Append)),
            (Escaped, ScanUnit::Newline(_)) => (Next, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Newline(_)) => (Next, Scan::Err(Cmd::Terminate)),
            (_, ScanUnit::Unknown(_)) => (Next, Scan::Ok(Cmd::Append)),
            (_, ScanUnit::Eof) => (Next, Scan::Err(Cmd::Terminate)),
        });

        map_tok(res, |str| {
            TokenKind::ByteString(ByteString::from_quot(quot, str))
        })
    }

    fn scan_operator(&mut self) -> TokenKind {
        let res = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Err(Cmd::Terminate), // `<='` seems like a weird thing to support...
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unknown(_) => Scan::Err(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |str| TokenKind::Operator(str))
    }

    fn scan_delimiter(&mut self, delim: Delimiter) -> TokenKind {
        self.advance_cursor();

        match delim {
            Delimiter::Paren(g) => TokenKind::Group(Group::Paren(g.into())),
            Delimiter::Brace(g) => TokenKind::Group(Group::Brace(g.into())),
            Delimiter::Bracket(g) => TokenKind::Group(Group::Bracket(g.into())),
            Delimiter::Semicolon => TokenKind::Semicolon,
            Delimiter::Comma => TokenKind::Comma,
        }
    }

    fn scan_whitespaces(&mut self, space: Space) -> TokenKind {
        // We cluster whitespace tokens into a single one for efficiency.
        // Importantly, we'd only do this for as long as the whitespace is the
        // same as the one we started out with.
        let res = self.take_while(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(s) if s == space => Scan::Ok(Cmd::Append),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unknown(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |(i, j)| {
            TokenKind::Ws(match space {
                Space::Space => Ws::Space { count: j - i },
                Space::Tab => Ws::Tab { count: j - i },
            })
        })
    }

    fn scan_newlines(&mut self) -> TokenKind {
        let res = self.take_while(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Unknown(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |(i, j)| TokenKind::Ws(Ws::Newline { count: j - i }))
    }
}

impl TokenStream {
    pub fn new(input: impl Into<String>) -> TokenStream {
        TokenStream {
            lexer: Lexer::new(input),
        }
    }
}

impl Iterator for Lexer {
    type Item = (Option<Absolute>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor.end() {
            return None;
        }

        let pos = self.current_position();

        // We don't call `next` or `advance_cursor` here yet because
        // we have to know the current index `i` to get a substring.
        // Advancing it now means the classic off by one error.
        let kind = match self.cursor.get() {
            ScanUnit::Unknown(_) => self.scan_unknown(),
            ScanUnit::Alpha(_) => self.scan_identifier(),
            ScanUnit::Digit(_) => self.scan_numeral(),
            ScanUnit::Quot(q) => self.scan_bytestring(q),
            ScanUnit::Operator(_) => self.scan_operator(),
            ScanUnit::Delimiter(delim) => self.scan_delimiter(delim),
            ScanUnit::Space(s) => self.scan_whitespaces(s),
            ScanUnit::Newline(_) => self.scan_newlines(),
            ScanUnit::Eof => {
                self.advance_cursor();
                TokenKind::Eof
            }
        };

        Some((self.offside.absolute_offside(), Token::new(kind, pos)))
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|(_, tok)| tok)
    }
}
