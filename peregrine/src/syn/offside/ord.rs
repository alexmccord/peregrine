use std::cmp::Ordering;

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
/// # let mut offside = OffsideBy::new();
/// # offside.add(&ScanUnit::Digit(Digit::Zero));
/// # #[derive(PartialEq)]
/// # struct FakeOrd<T>(T);
/// # impl<T: PartialOffsideOrd> PartialOffsideOrd for FakeOrd<T> {
/// #   fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
/// #     self.0.partial_cmp_offside(&other.0)
/// #   }
/// # }
/// # impl<T: PartialOffsideOrd + PartialEq> PartialOrd for FakeOrd<T> {
/// #    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
/// #       self.partial_cmp_offside(other).map(|ord| match ord {
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
/// let rhs = offside.absolute();
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
        self.partial_cmp_offside(other) == Some(Indentation::Dedented)
    }

    fn is_aligned_with(&self, other: &Rhs) -> bool {
        self.partial_cmp_offside(other) == Some(Indentation::Aligned)
    }

    fn is_more_indented_than(&self, other: &Rhs) -> bool {
        self.partial_cmp_offside(other) == Some(Indentation::Indented)
    }
}

/// If you have an implementation that never returns [`None`], you can implement
/// [`OffsideOrd`] for it and have [`PartialOffsideOrd`] be expressed in terms
/// of the former one to guarantee at compile-time that it is not partial.
pub trait OffsideOrd: PartialOffsideOrd {
    fn cmp_offside(&self, other: &Self) -> Indentation;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Indentation {
    Dedented = -1,
    Aligned = 0,
    Indented = 1,
}

impl Indentation {
    /// Indentation forms a hierarchy, hence if you call [`Indentation::then`]
    /// where `self` is [`Indentation::Dedented`] or [`Indentation::Indented`],
    /// it will return [`None`] because at that point we've lost the hierarchy.
    pub fn then(self, other: Indentation) -> Option<Indentation> {
        self.and_then(Some(other))
    }

    /// Indentation forms a hierarchy, hence if you call [`Indentation::then`]
    /// where `self` is [`Indentation::Dedented`] or [`Indentation::Indented`],
    /// it will return [`None`] because at that point we've lost the hierarchy.
    pub fn and_then(self, other: Option<Indentation>) -> Option<Indentation> {
        match self {
            Indentation::Dedented => None,
            Indentation::Aligned => other,
            Indentation::Indented => None,
        }
    }

    pub fn then_with<F: FnOnce() -> Indentation>(self, f: F) -> Option<Indentation> {
        self.then(f())
    }

    pub fn and_then_with<F: FnOnce() -> Option<Indentation>>(self, f: F) -> Option<Indentation> {
        self.and_then(f())
    }

    pub fn from_ord(ordering: Ordering) -> Indentation {
        Indentation::from(ordering)
    }

    pub fn into_ord(self) -> Ordering {
        Ordering::from(self)
    }
}

impl PartialOffsideOrd for Indentation {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        self.and_then(Some(*other))
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for &[T] {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        self.iter()
            .zip(other.iter())
            .map(|(lhs, rhs)| lhs.partial_cmp_offside(rhs))
            .try_fold(Indentation::Aligned, Indentation::and_then)?
            .then(Indentation::from_ord(self.len().cmp(&other.len())))
    }
}

impl<T: PartialOffsideOrd> PartialOffsideOrd for Vec<T> {
    fn partial_cmp_offside(&self, other: &Self) -> Option<Indentation> {
        PartialOffsideOrd::partial_cmp_offside(&self.as_slice(), &other.as_slice())
    }
}

impl<T, Rhs> PartialOffsideOrd<Option<Rhs>> for Option<T>
where
    T: PartialOffsideOrd<Rhs>,
{
    fn partial_cmp_offside(&self, other: &Option<Rhs>) -> Option<Indentation> {
        match (self, other) {
            (Some(lhs), Some(rhs)) => lhs.partial_cmp_offside(&rhs),
            (_, _) => None,
        }
    }
}

impl<T: ?Sized, Rhs: ?Sized> PartialOffsideOrd<&Rhs> for &T
where
    T: PartialOffsideOrd<Rhs>,
{
    fn partial_cmp_offside(&self, other: &&Rhs) -> Option<Indentation> {
        PartialOffsideOrd::partial_cmp_offside(&**self, &*other)
    }
}

impl<T: ?Sized, Rhs: ?Sized> PartialOffsideOrd<&mut Rhs> for &mut T
where
    T: PartialOffsideOrd<Rhs>,
{
    fn partial_cmp_offside(&self, other: &&mut Rhs) -> Option<Indentation> {
        PartialOffsideOrd::partial_cmp_offside(&**self, &*other)
    }
}

impl From<Ordering> for Indentation {
    fn from(value: Ordering) -> Self {
        match value {
            Ordering::Less => Indentation::Dedented,
            Ordering::Equal => Indentation::Aligned,
            Ordering::Greater => Indentation::Indented,
        }
    }
}

impl From<Indentation> for Ordering {
    fn from(value: Indentation) -> Self {
        match value {
            Indentation::Dedented => Ordering::Less,
            Indentation::Aligned => Ordering::Equal,
            Indentation::Indented => Ordering::Greater,
        }
    }
}
