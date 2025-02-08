#[macro_export]
macro_rules! newindex {
    (pub($vis:tt) $name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub($vis) struct $name(usize);

        $crate::newindex_impls!($name);
    };
    (pub $name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub struct $name(usize);

        $crate::newindex_impls!($name);
    };
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        struct $name(usize);

        $crate::newindex_impls!($name);
    }
}

#[macro_export]
macro_rules! newindex_impls {
    ($name:ident) => {
        impl $crate::idx::Idx for $name {
            fn new(index: usize) -> Self {
                Self(index)
            }

            fn index(self) -> usize {
                self.0
            }
        }

        impl ::core::fmt::Display for $name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::write!(f, "{}({})", stringify!($name), self.0)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::idx;

    idx::newindex!(pub(crate) PubInCrateId);
    idx::newindex!(pub PubId);
    idx::newindex!(PrivateId);
}
