# Type system

Peregrine's type system plans to support dependent types, parametric polymorphism, and functions over types. In other words, calculus of constructions, or in terms of where it falls on the lambda cube, $\lambda C$.

## Cumulative universes

Cumulativity refers to the idea that a term in some universe $U_\alpha$ is also a term in $U_{\alpha+1}$. One example is a function in $U_1$ can use some term in $U_0$ to produce a term in $U_\alpha$ for any $\alpha \le 1$, also known as dependent types. Cumulativity generalizes this even further, but not without violating Russell's paradox: any $U_\alpha$ cannot _know_ about any term in $U_{\alpha+1}$, hence the names "small" and "large" types, which Peregrine uses to enforce the naming convention.

In general, most of the time we live in $U_0$ (values) and $U_1$ (types) with occassional visits in $U_2$ (types of types), and beyond that is where people of certain sorts are.

## Dependent types and annotating AST.

One issue I ran into in Haskell is the classic "how to annotate your AST" problem. Suppose you write the naive thing:

```hs
data Expr where
  Var :: Symbol         -> Expr
  Lam :: Expr   -> Expr -> Expr
  App :: Expr   -> Expr -> Expr
```

You find yourself at a fork in the road with arbitrary directions to go in and none of them have signposts indicating the direction.

You could parameterize `Expr` with `a` and put it on every data constructors.

```hs
data Expr a where
  Var :: a -> Symbol           -> Expr a
  Lam :: a -> Expr a -> Expr a -> Expr a
  App :: a -> Expr a -> Expr a -> Expr a
```

This isn't so bad, except sometimes you just don't care about `a` at all, for instance you might be _building_ this graph for the first time from a string, i.e. parsing.

Another approach is, again by parameterizing `Expr` with `a`, except you replace recursive references with `a`. Then you introduce some `Fix` type.

```hs
data ExprF a where
  Var :: Symbol ->      Expr a
  Lam :: a      -> a -> Expr a
  App :: a      -> a -> Expr a

newtype Fix f = Fix { unFix :: f (Fix f) }

type Expr = Fix ExprF
```

This allows us to use `ExprF` with various kinds of algebraic functions, like F-algebras. Unfortunately, this approach pushes leaky abstractions like `Fix (unFix)`, `Free (Pure, Free)`, `Cofree (:<)` onto your algebras as well.

The third approach, is to add one extra data constructor, called `Ann`.

```hs
data Expr a where
  Var :: Symbol           -> Expr a
  Lam :: Expr a -> Expr a -> Expr a
  App :: Expr a -> Expr a -> Expr a
  Ann :: a      -> Expr a -> Expr a
```

This all, to me, seems to be stemmed by the lack of a principled way to view _types_ as nothing but data constructors in the next universe. For instance, recursion in lambda calculus is nothing but a `Y` combinator, whether it be functions or data.

Suppose that in the type system, the original `Expr` datatype we wrote in the first place was just a `Y` combinator of `Expr` all along. That is, we could write the `y` combinator here.

```hs
y :: (a -> a) -> a
y = \f. (\x. f (x x)) (\x. f (x x))
```

Then our data type `Expr` here

```hs
data Expr where
  Var :: Symbol         -> Expr
  Lam :: Expr   -> Expr -> Expr
  App :: Expr   -> Expr -> Expr
```

is actually, from the lens of the compiler, written as

```hs
-- Strawman: $Expr is just some metavariable
-- that users will never see.
data $Expr a where
  Var :: Symbol      -> $Expr a
  Lam :: a      -> a -> $Expr a
  App :: a      -> a -> $Expr a

-- This is what the user gets when they refer
-- to Expr outside of the declaration of Expr.
Expr :: Type
Expr = y Expr
```

Then, going back to the "types are data constructors in the next universe" realization, we'd be allowed to write things such as:

```hs
FAlgebra :: Type -> Type -> Type
FAlgebra (y f) a = f a -> a
```

Where `(y f)` is pattern matching on the provided type, extracting `f`, and then returns a type with `a` applied on `f`. Here, we can then write an F-algebra using our dumb naive type we wrote in the first place, avoiding all boilerplate or leaky abstractions that a more rigid type system induces.

```hs
depth :: FAlgebra Expr Int
depth (Var _) = 0
depth (Lam x e) = 1 + max x e -- x :: Int and e :: Int
depth (App f e) = 1 + max f e -- f :: Int and e :: Int
```

And the function application `FAlgebra Expr Int` matches the pattern in the equation `FAlgebra (y f) a` by unification: `(y $Expr) Int` and returns the type `$Expr Int -> Int`. The `(y $Expr)` pattern replaces `Expr` with `Int` as the term of the data structure.
