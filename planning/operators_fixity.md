# Fixity of operators

There's several approaches done by most programming languages:

1. Hardcoding fixity of operators by the language spec
2. Specifying the precedence with a number, e.g. `infix 5`
3. Specifying the associativity, e.g. `infixl` and `infixr`
4. Requiring explicit parentheses, struct or lax

The most common is obviously the first, which is pretty tolerable for the most part. The mistake usually occurs when the language picks a different syntax and doesn't adjust the fixity in accordance to human ambiguity. A human always reads `!x == y` as `(!x) == y` unambiguously, but not so for `not x == y`, which can be read as `(not x) == y` or `not (x == y)` and either interpretation does not yield the same result. Worse, side effects could be evaluated in a different order, too.

## Fixity design in `L`

The ideal design is one where you define your operators _relative_ to another.

Suppose we've discovered numbers:

```
enum Nat where
  Z
  S Nat
```

And we realized the idea to "lump" numbers in different ways, which we will call "adding," and denote that action with an operator `+` called plus. From observational evidence of adding things together scientifically, we have a few axioms for which we implement on `Nat + Nat`.

```
trait x + y where
  Add x y = x + y

impl Nat + Nat where
  x + Z   = x
  x + S y = S (x + y)
```

Then after using the `+` for a bit while, one wishes for a function doing the repetition for us. We could denote this operator `*` called multiply.

```
trait x * y where
  Mul x y = x * y

impl Nat * Nat where
  x * Z   = Z
  x * S y = x + (x * y)
```

And in `Nat * Nat` we can see that it is expressed in terms of `Nat + Nat`, so it must be the case that `Nat * Nat` has a higher precedence over `Nat + Nat`. We can also see how associativity is emergent based on the implementation:

$$
\def\z{\mathrm{Z}}
\def\s#1{\mathrm{S}#1}

\begin{align*}
\s\s\z + (\s\s\z * \s\s\s\z)
    &= \s\s\z + \s\s\z + (\s\s\z * \s\s\z) \\
    &= \s\s\z + \s\s\z + \s\s\z + (\s\s\z * \s\z) \\
    &= \s\s\z + \s\s\z + \s\s\z + \s\s\z + (\s\s\z * \z) \\
    &= \s\s\z + \s\s\z + \s\s\z + \s\s\z + \z \\
    &= \s\s\z + \s\s\z + \s\s\z + \s\s\z \\
    &= \s\s\z + \s\s\z + \s\s\s\z + \s\z \\
    &= \s\s\z + \s\s\z + \s\s\s\s\z + \z \\
    &= \s\s\z + \s\s\z + \s\s\s\s\z \\
    &= \s\s\z + \s\s\s\z + \s\s\s\z \\
    &= \s\s\z + \s\s\s\s\z + \s\s\z \\
    &= \s\s\z + \s\s\s\s\s\z + \s\z \\
    &= \s\s\z + \s\s\s\s\s\s\z + \z \\
    &= \s\s\z + \s\s\s\s\s\s\z \\
    &= \s\s\s\z + \s\s\s\s\s\z \\
    &= \s\s\s\s\z + \s\s\s\s\z \\
    &= \s\s\s\s\s\z + \s\s\s\z \\
    &= \s\s\s\s\s\s\z + \s\s\z \\
    &= \s\s\s\s\s\s\s\z + \s\z \\
    &= \s\s\s\s\s\s\s\s\z + \z \\
    &= \s\s\s\s\s\s\s\s\z \\
\end{align*}
$$

As you can see, this is pretty inefficient. This is where isomorphism classes come in.

```
impl Nat <=> OpSem.UInt64 where
  Z   = 0
  S n = 1 + n
```
