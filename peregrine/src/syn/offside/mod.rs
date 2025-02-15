//! Thinking about how I'd do layout rules in a more rigid manner than Haskell
//! to avoid some of its problems. I think this has to be done in two places,
//! both the lexer and the parser.
//!
//! Reminder: the fact they both work in tandem should be an implementation
//! detail. Ideally, you should be able to see _all_ the relevant tokens
//! regardless of the state of the parser, so we want to avoid exposing things
//! like `Lexer::next_nontrivia` or something, i.e. there is only one way to
//! advance the state of the lexer.
//!
//! Lexer emits a `Ws` token that represents the simple fact that we have
//! encountered some whitespace. Whether it gets skipped or not is of no
//! relevance to the lexer.
//!
//! The lexer consume as many whitespace as possible, so long as the kind of
//! whitespace in question matches the the first one we consumed. This way we
//! can preserve a structured stack of whitespace kinds. Normalizing them away
//! is a bad idea.
//!
//! We will also need to record the number of bytes occupied by text. This would
//! be another integer stored in `Space`. This solves a few problems.
//!
//! Then with all this information collected, the parser should now be ready to
//! preserve a stack of whitespace required to visually align expressions even
//! against adversarial input like when the code is being edited that's just
//! completely and utterly wrong, and even from whitespace-free programmers.
//!
//! The way whitespace tokens are structured tells us three key data:
//!
//! 1. A stack of whitespace necessary to preserve the offside rule.
//! 2. Each offside rule tells us the length of monospace followed by elastic.
//! 3. The byte offset difference occupied by text and leading whitespace.
//!
//! This means the snippet above would tokenize as the following:
//! ```hs
//! foo = do expr1\n
//!   ^^^^ ^^    ^^
//!   1234 56    78
//!          expr2
//!         ^    ^
//!         9    10
//! ```
//! ```rs
//! Relative { monospace: 3, elastic: 0 } // (1)
//! Relative { monospace: 1, elastic: 0 } // (2)
//! Relative { monospace: 1, elastic: 0 } // (3)
//! Relative { monospace: 1, elastic: 0 } // (4)
//! Relative { monospace: 2, elastic: 0 } // (5)
//! Relative { monospace: 1, elastic: 0 } // (6)
//! Relative { monospace: 5, elastic: 0 } // (7)
//! Relative { monospace: 0, elastic: 0 } // (8)
//! Relative { monospace: 9, elastic: 0 } // (9)
//! Relative { monospace: 5, elastic: 0 } // (10)
//! ```
//! So, for as long as the absolute offside is equal to the previous line's
//! absolute offside, we know we're in the same layout, where the "previous
//! line" really means any line that contained any substance at all, not just
//! whitespace and newlines. But this information is tracked by the parser, not
//! the lexer. Again, the entire purpose of the lexer is to be a token stream
//! with no knobs.
//!
//! The `Ws` only exists to serve lossless parsing and support mixed tabs and
//! spaces which can actually happen.
//!
//! Offsides in Peregrine is stored as a stack of monospace and elastic bytes.
//!
//! What this means is when you write any expression that uses tabs, we have to
//! track them correctly. For instance, where `<ts>` is a hard tabstop:
//! ```hs
//! foo "<ts>" | expr = expr
//!      <ts>  | otherwise = expr
//! ```
//! In Haskell, such tabs are normalized to modulo 8 spaces minus 1. This means
//! you can write things that are visually out of alignment. We'd really like to
//! avoid this problem. Otherwise, you have this, which is a valid parse (see
//! note) in Haskell since they normalize whitespace.
//! ```hs
//! foo "<ts>" | expr = expr
//!              | otherwise = expr
//! ```
//! Note: you can't even have `"<ts>"` in Haskell. The language will not even
//! lex it. But suppose you could, then this would be a problem. And arguably,
//! we should probably support it because you might want to paste some snippet
//! that used tabstops in it into a string.
//!
//! One option is to prevent the use of mixed tabs and spaces, but this doesn't
//! work out nicely as one would hope it does because of text editor settings.
//! It's just not UX friendly.
//!
//! Another option is to just... ban tabs entirely, and use spaces. But I
//! predict people will complain about this problem. Same problem as above.
//!
//! Or... we just accept the annoyances and put in the elbow grease to support
//! it. This has the added benefit of being able to track the expected
//! whitespace kind correctly, with the correct source span.
//!
//! We could even infer the _length_ of whitespace that was supposed to be
//! replaced by a hard tabstop in a future pass by measuring a few key data:
//!
//! 1. How many tabs did we expect?
//! 2. How many tabs did we actually have, _ahead_ of the incorrect whitespace?
//!    1. If so, determine the number of spaces between tabs.
//!    2. If not, determine the whitespace distance to the nearest anchor point.
//!
//! The idea is that you can always do a set intersection between the two
//! erroneous lines and still be able to pinpoint the source span that needs
//! replacing to fix it.
//!
//! That said, there's still the inverse that we have to think about too:
//! ```hs
//! foo "<ts>"<ts>| expr = expr
//! <ts><ts><ts>| otherwise = expr
//! ```
//! In this case, we have to suggest a fix to replace the first `<ts>` on the
//! second line to 5 whitespaces, and insert a space between second and third
//! `<ts>`. This will visually align the bars.
//! ```hs
//! foo "<ts>"<ts>| expr = expr
//!      <ts> <ts>| otherwise = expr
//! ```
//! We could even "fix" it for the user behind the scenes if the analysis has
//! confidence in it, and then continue onward with other passes, even if the
//! syntax isn't completely well-formed. Note that the error _did_ happen, so it
//! won't compile, but at least you won't be dumbfounded.
//!
//! Culturally speaking, Peregrine will not support such willful ignorance of
//! the issues that mixing tabs and spaces brings, but we still need to be able
//! to lex and parse it correctly for the purpose of devtooling and to correct
//! the mistake with fixits.
//!
//! As an added benefit, we can even sense whether an operator has been
//! separated by a whitespace token or not, for the purpose of disambiguating
//! between unary and binary operators, or even indexing in a record vs function
//! composition.
//!
//! Layout rules describes the _intended_ structure of the text which gets
//! encoded directly in the AST. This means when you write things like:
//! ```hs
//! do expr1
//!    expr2
//! expr3
//! ```
//! The parser has to capture the information about each expressions and to work
//! with these layout rules to be able to make some determination about the
//! state of the text and their alignment in relation to another AST node.
//!
//! For instance, using the example above, how would the parser decide that
//! `expr2` is not a function application over the term `expr3`? Similarly,
//! consider the following:
//! ```hs
//! do expr1
//!      expr2
//!      expr3
//!    expr4
//! ```
//! This is also equally as valid because `expr2` and `expr3` are indented at a
//! level higher than `expr1` and as such is parsed as `expr1 expr2 expr3` on a
//! single line and `expr4` aligns with `expr1` which ends the parsing of
//! function applications for `expr1`.
//!
//! Another wrench is when the text is misaligned, which happens all the time
//! while editing code. We'd like to have all the necessary information to
//! recover from such ugly situations, e.g.
//! ```hs
//! f x | x == 5   = Just x
//!    | otherwise = Nothing
//!    ^ -- misaligned!
//! ```
//! For devtooling reasons, we'd really like to still get a parse tree out of
//! this and _at least_ be able to obtain information to report decent errors,
//! and perhaps in some cases, fix it without blocking all other analysis
//! passes.
//!
//! Another wrench, sometimes the function being applied can be more indented
//! than the argument term! For instance, this is another valid parse for an
//! expression `return ()`.
//! ```hs
//! main : IO ()
//! main = return
//!   ()
//! ```
//! At first glance it might seem strange but this layout actually makes more
//! sense when you consider the ergonomics where a user might want to have some
//! initial seed value and chain them using the same operator over multiple
//! lines.
//!
//! But this is only one small part of the larger picture surrounding a layout
//! sensitive language syntax: how does this interact with Unicode? The answer
//! is: badly. Very badly. There's a few footguns to watch out for.
//!
//! 1. One byte does not equal one codepoint.
//! 2. One codepoint does not equal one grapheme.
//! 3. One grapheme does not equal one display width.
//! 4. The display width is not defined except for a subset of Unicode.
//! 5. Even those defined by Unicode, not all implementations follow it.
//! 6. The display width for any arbitrary set of graphemes can depend on:
//!    1. The user's font.
//!    2. The text rendering backend.
//!    3. Whether the user is using a terminal or UI-based text editor.
//!    3. And all other factors unbeknownst to me.
//! 7. Some emojis are 27 bytes long, and Unicode allows arbitrarily many emoji
//!    sequences.
//!
//! Put simply: perfect layout parsing with Unicode support is undecidable. Even
//! if you could get the display width of a grapheme, you can't rely on it to be
//! consistent for everyone else! This is unfortunate because the actual metric
//! for layout parsing _is_ the display width.
//!
//! So, the only real way out is to make a pragmatic decision: strings must be
//! ASCII if and only if they are whitespace significant. The consequences of
//! naive implementation of layout parsing while trying to be Unicode friendly
//! is too costly to accept risks. If done incorrectly, your program could be
//! parsed in a different way than the visual representation, which is
//! unacceptable if one hopes to claim that Peregrine is production-ready, safe,
//! and secure by default.
//!
//! Also, Peregrine differs from Haskell in some constructs, particularly around
//! their layout flexibility, e.g. lists, tuples, and data constructors.
//! ```hs
//! data Foo =
//!     Bar
//!   | Baz
//!    | Quux
//!  | Frob
//! ```
//! Here, we enforce two more rules:
//! 1. If `=` is followed by a newline, a `|` is expected.
//! 2. If a data constructor is defined on the next line, we push a new layout
//!    block.
//!
//! So Peregrine's valid variations are:
//! ```hs
//! -- Cannot define a new data constructor
//! -- at an indentation less than `| Bar`.
//! data Foo =
//!   | Bar
//!   | Baz
//!   | Quux
//!   | Frob
//!
//! -- Cannot define a new data constructor
//! -- at an indentation less than `| Baz`.
//! data Foo = Bar
//!          | Baz
//!          | Quux
//!          | Frob
//!
//! -- Cannot define a new data constructor
//! -- at an indentation less than `= Bar`.
//! data Foo
//!   = Bar
//!   | Baz
//!   | Quux
//!   | Frob
//!
//! -- Cannot define a new data constructor
//! -- at an indentation less than `| Quux`.
//! data Foo = Bar  | Baz
//!          | Quux | Frob
//!
//! -- Cannot define a new data constructor
//! -- at an indentation less than `| Quux`.
//! data Foo = Bar  | Baz
//!   | Quux | Frob
//! ```

use std::cmp::Ordering;

use crate::syn::cursor::ScanUnit;

mod tests;

#[derive(Debug)]
pub struct Offside {
    /// When the line contains a non-ASCII character, then the offside rule
    /// cannot be applied to that line.
    nonascii: bool,
    /// We don't make this an `Option` just to preserve the memory buffer even
    /// when `nonascii` is set to true. Always check with `nonascii` field
    /// first!
    absolute: Absolute,
}

/// Relative offside is a specific column tracking the width-wise length of a
/// specific column in such a way that guarantees visual alignment. It records
/// in pairs the accumulated number of monospace and elastic bytes.
/// ```hs
/// foo x y "<ts>" = expr
/// |^^^^^^^^^^^^|^^^^^^^
/// 1            2
/// ```
/// Such that we get the following data:
/// ```rs
/// Relative { monospace: 9, elastic: 1 } // 1
/// Relative { monospace: 8, elastic: 0 } // 2
/// ```
///
/// This is emitted on a per token basis.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Relative {
    /// Number of codepoints whose display width is monospaced.
    monospace: u16,
    /// Tabs only.
    elastic: u16,
}

/// The action of stacking a bunch of [`Relative`] gives us an absolute offside.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Absolute(AbsoluteOffside);

#[derive(Debug, PartialEq, Eq, Clone)]
enum AbsoluteOffside {
    Zero,
    One([Relative; 1]),
    Two([Relative; 2]),
    // If we have one more `Relative` in this state, we switch to Heap because
    // `sizeof(Relative) * 4 > sizeof(Vec<Relative>)`.
    Three([Relative; 3]),
    Heap(Vec<Relative>),
}

#[derive(Debug, Clone, Copy)]
pub enum Indentation {
    Dedented,
    Aligned,
    Indented,
}

/// Why not [`PartialOrd`]? Because [`Option<T>`] where `T: PartialOrd` returns
/// true, but that's wrong for the problem domain in this case!
/// ```
/// let x = None;
/// let y = Some(5);
///
/// assert_eq!(x < y, true);  // Bad! We want false!
/// assert_eq!(x > y, false); // Good! ... or is it?
/// ```
/// This isn't what we want if we replace `5` with an [`Absolute`], because
/// [`None`] means something different: a line with no possible way to compute
/// an offside.
///
/// Hence we have a different trait to deal with it and delete this footgun.
/// ```
/// # use std::ops::Deref;
/// # use std::cmp::Ordering;
/// # use peregrine::syn::offside::*;
/// # use peregrine::syn::cursor::*;
/// # let mut offside = Offside::new();
/// # offside.push_scan_unit(ScanUnit::Digit(Digit::Zero));
/// # #[derive(PartialEq)]
/// # struct FakeOrd<T>(T);
/// # impl<T: PartialOffsideOrd> PartialOffsideOrd for FakeOrd<T> {
/// #   fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
/// #     self.0.partial_cmp_offside(&other.0)
/// #   }
/// # }
/// # impl<T: PartialOffsideOrd + PartialEq> PartialOrd for FakeOrd<T> {
/// #    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
/// #       self.partial_cmp_offside(&other).map(|ord| match ord {
/// #         Indentation::Dedented => Ordering::Less,
/// #         Indentation::Aligned => Ordering::Equal,
/// #         Indentation::Indented => Ordering::Greater,
/// #       })
/// #    }
/// # }
/// # impl<T> Deref for FakeOrd<T> {
/// #   type Target = T;
/// #   fn deref(&self) -> &Self::Target { &self.0 }
/// # }
/// let lhs = None;
/// let rhs = offside.absolute_offside();
/// # let rhs = rhs.map(FakeOrd);
///
/// assert_eq!(lhs < rhs, true); // Unfortunate.
/// assert_eq!(lhs.is_less_indented_than(&rhs), false);
/// ```
/// Honestly, this is actually a great example of why I think `Maybe` and other
/// higher kinded types should not be `PartialOrd` or `Ord` in the new standard
/// library, because ordering is a domain-specific detail.
///
/// Perhaps we will redesign how `Ord` operators interact with `Ord`. What that
/// would look like remains to be seen, but I reject the notion that `Maybe`,
/// `Either`, and other types like them from being instances of `PartialOrd` and
/// `Ord`.
///
/// Note that if any one of these methods returns `false`, it does not imply the
/// inverse for any other ones:
///
/// 1. [`PartialOffsideOrd::is_less_indented_than`]
/// 2. [`PartialOffsideOrd::is_aligned_with`]
/// 3. [`PartialOffsideOrd::is_more_indented_than`]
///
/// Meaning, if one of them returns false, it is entirely possible for all of
/// them to still return false, _even_ if you swap the arguments around. If you
/// needed to handle other situations, then you must use
/// [`PartialOffsideOrd::partial_cmp_offside`].
pub trait PartialOffsideOrd<Rhs: ?Sized = Self> {
    fn partial_cmp_offside(&self, other: &Rhs) -> Option<Indentation>;

    fn is_less_indented_than(&self, other: &Rhs) -> bool {
        matches!(self.partial_cmp_offside(other), Some(Indentation::Dedented))
    }

    fn is_aligned_with(&self, other: &Rhs) -> bool {
        matches!(self.partial_cmp_offside(other), Some(Indentation::Aligned))
    }

    fn is_more_indented_than(&self, other: &Rhs) -> bool {
        matches!(self.partial_cmp_offside(other), Some(Indentation::Indented))
    }
}

/// If you have an implementation that never returns [`None`], you can implement
/// [`OffsideOrd`] for it and have [`PartialOffsideOrd`] be expressed in terms
/// of the former one to guarantee at compile-time that it is not partial.
pub trait OffsideOrd: PartialOffsideOrd {
    fn cmp_offside(&self, other: &Self) -> Indentation;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Width {
    Monospace,
    Elastic,
}

impl Offside {
    pub fn new() -> Offside {
        Offside {
            nonascii: false,
            absolute: Absolute::new(),
        }
    }

    pub fn absolute_offside(&self) -> Option<&Absolute> {
        if self.nonascii {
            return None;
        }

        Some(&self.absolute)
    }

    pub fn push_scan_unit(&mut self, scan_unit: ScanUnit) {
        match scan_unit {
            ScanUnit::Newline(_) => {
                self.nonascii = false;
                self.absolute.reset();
            }
            ScanUnit::Unknown(u) if !u.is_ascii() => {
                self.nonascii = true;
                self.absolute.reset();
            }
            _ if self.nonascii => return,
            _ => match scan_unit.char_len() {
                Some(_) => self.absolute.push(Width::Monospace),
                None => self.absolute.push(Width::Elastic),
            },
        }
    }
}

impl Relative {
    fn new(width: Width) -> Relative {
        let (monospace, elastic) = match width {
            Width::Monospace => (1, 0),
            Width::Elastic => (0, 1),
        };

        Relative { monospace, elastic }
    }

    fn add(self, width: Width) -> Option<Relative> {
        let res = match width {
            Width::Monospace if self.elastic == 0 => Some((1, 0)),
            Width::Monospace => None,
            Width::Elastic => Some((0, 1)),
        };

        Some(Relative {
            monospace: self.monospace + res?.0,
            elastic: self.elastic + res?.1,
        })
    }
}

impl Absolute {
    pub fn new() -> Absolute {
        Absolute(AbsoluteOffside::Zero)
    }

    fn push(&mut self, width: Width) {
        use AbsoluteOffside::*;

        // Cluster as much as possible into a single offside rule.
        //
        // If we have a Width::Monospace:
        // 1. If the last offside rule is not elastic, then we increment the
        //    monospaced integer in the offside.
        // 2. Otherwise we push a new offside rule with monospaced set to 1.
        //
        // If we have a Width::Elastic, then simply increment the elastic
        // integer in the offside. This alone is enough to cause a new entry in
        // the stack if the next width is monospaced.
        //
        // This means we efficiently pack the count of monospaced and elastic
        // bytes.

        if let Some(last) = self.last_mut() {
            if let Some(added) = last.add(width) {
                *last = added;
                return;
            }
        }

        let last = Relative::new(width);
        match self.0 {
            Zero => self.0 = One([last]),
            One([r1]) => self.0 = Two([r1, last]),
            Two([r1, r2]) => self.0 = Three([r1, r2, last]),
            Three([r1, r2, r3]) => self.0 = Heap(Vec::from([r1, r2, r3, last])),
            Heap(ref mut relatives) => relatives.push(last),
        }
    }

    pub fn reset(&mut self) {
        match self.0 {
            // Once in Heap, we never go back lest we lose the memory buffer.
            AbsoluteOffside::Heap(ref mut relatives) => relatives.clear(),
            _ => self.0 = AbsoluteOffside::Zero,
        }
    }

    pub fn as_slice(&self) -> &[Relative] {
        match &self.0 {
            AbsoluteOffside::Zero => &[],
            AbsoluteOffside::One(rs) => rs,
            AbsoluteOffside::Two(rs) => rs,
            AbsoluteOffside::Three(rs) => rs,
            AbsoluteOffside::Heap(vec) => &vec,
        }
    }

    pub fn as_mut_slice(&mut self) -> &mut [Relative] {
        match &mut self.0 {
            AbsoluteOffside::Zero => &mut [],
            AbsoluteOffside::One(rs) => rs,
            AbsoluteOffside::Two(rs) => rs,
            AbsoluteOffside::Three(rs) => rs,
            AbsoluteOffside::Heap(vec) => vec.as_mut_slice(),
        }
    }

    fn last_mut(&mut self) -> Option<&mut Relative> {
        self.as_mut_slice().last_mut()
    }
}

impl Indentation {
    pub fn then(self, other: Indentation) -> Indentation {
        match self {
            Indentation::Aligned => other,
            _ => self,
        }
    }

    pub fn then_with<F: FnOnce() -> Indentation>(self, f: F) -> Indentation {
        self.then(f())
    }
}

impl PartialOffsideOrd for Indentation {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        Some(self.then(*other))
    }
}

impl OffsideOrd for Indentation {
    fn cmp_offside(&self, other: &Self) -> Indentation {
        self.then(*other)
    }
}

impl PartialOffsideOrd for Relative {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        Some(self.cmp_offside(other))
    }
}

impl OffsideOrd for Relative {
    fn cmp_offside(&self, other: &Self) -> Indentation {
        let mono_ord = self.monospace.cmp(&other.monospace);
        let elastic_ord = self.elastic.cmp(&other.elastic);

        match mono_ord.then(elastic_ord) {
            Ordering::Less => Indentation::Dedented,
            Ordering::Equal => Indentation::Aligned,
            Ordering::Greater => Indentation::Indented,
        }
    }
}

impl PartialOffsideOrd for Absolute {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        self.as_slice().partial_cmp_offside(&other.as_slice())
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for Option<T> {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        match (self, other) {
            (Some(lhs), Some(rhs)) => lhs.partial_cmp_offside(rhs),
            (_, _) => None,
        }
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for Vec<T> {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        self.as_slice().partial_cmp_offside(&other.as_slice())
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for &[T] {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        let mut iter = self.iter().zip(other.iter());
        let last = iter
            .next_back()
            .and_then(|(lhs, rhs)| lhs.partial_cmp_offside(rhs));

        if iter.any(|(lhs, rhs)| !lhs.is_aligned_with(rhs)) {
            return None;
        }

        let implied_indent = match self.len().cmp(&other.len()) {
            Ordering::Less => Indentation::Dedented,
            Ordering::Equal => Indentation::Aligned,
            Ordering::Greater => Indentation::Indented,
        };

        last.partial_cmp_offside(&Some(implied_indent))
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for &T {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        (*self).partial_cmp_offside(*other)
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for &mut T {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        (&**self).partial_cmp_offside(&**other)
    }
}
