# Isomorphism classes

In most languages that doesn't check for totality will not be able to guarantee it. Of those that does, does not really lean on it, as far as I can tell.

In `L`, I plan on using isomorphism classes as an analogy to translating types in `L` onto the ideal IR type. The keyword here is "ideal" which is compiler defined. Although this may change as well as I learn more, or if there's a better approach I have yet to realize.

## Types of isomorphisms

1. Bijection, an isomorphism between two _sets_.
1. Isometry, an isomorphism between two _metric spaces_.
3. Homeomorphism, an isomorphism between two _topological spaces_.
2. Isomorphism, an isomorphism between two _algebraic structures_.
5. And so many more.

Since there are different types of isomorphisms, we need some generalization. Category theory gives us the language we need to provide this. The isomorphism class that we refer to are the one in the language of category theory, not the ones from various fields of mathematics, although `L` will need to implement some of them.

So we (roughly speaking) define our `<=>` trait:

```
trait (a: T) <=> (b: T) where
  from (a <=> b) = a
  to   (a <=> b) = b

  a <=> b
```

And with this definition, any `a` and `b` with a common supertrait can implement the `<=>` trait. This means if `a` is some set not endowed by some algebraic structure and `b` is some other set endowed with an algebraic structure, `L` should issue an error here.

## In practical use

For instance, you can define the `Nat` type to be isomorphic to some `OpSem` type representing that value.

```
enum Nat where
  Z
  S Nat

-- Endows `Nat` with an algebraic structure of `+`.
impl Nat + Nat where
  x + Z   = x
  x + S y = S (x + y)

-- Suppose for now that `OpSem.UInt32` only implements `+`.
-- Then by substition of `Nat <=> OpSem.UInt32` we get our trait type
--
--   (Nat: (Nat + Nat)) <=> (OpSem.UInt32: (OpSem.UInt32 + OpSem.UInt32))
--
-- Indeed, `Nat + Nat` is incompatible with `OpSem.UInt32 + OpSem.UInt32`.
-- However, the two traits are a subtrait of `a + b`, which passes the subtyping
-- rules, satisfying the trait bounds required by the `<=>` trait.
--
-- Then implement the algebraic isomorphism in terms of small-step rules.
impl Nat <=> OpSem.UInt32 where
  Z   <=> 0
  S n <=> 1 + n
```

With a complete implementation provided, the onus is on the compiler to prove isomorphisms do indeed hold using operational semantics. If it is not proven, an error will be issued. Say that it holds, then the compiler can select the ideal runtime representation within the isomorphism class if it was so compelled to.

Since it is an isomorphism _class_, you can also have `Nat` be isomorphic to arbitrarily many others, as long as there is a proof that they are.

```
impl Nat <=> OpSem.UInt64 where
  Z   <=> 0
  S n <=> 1 + n
```

## Unresolved questions

1. Can we use this to decide what runtime representation to use that gives maximum benefit?
2. Can we use this to solve the [ABI horror show](https://faultlore.com/)?
3. How to solve the many string types problem?
4. Can this alleviate some kind of compatibility problems, backwards and forwards?

## Implementation of the `<=>` trait

1. Need a way to talk about "is the function injective"
2. Need a way to talk about "is the function surjective"
3. Need a way to talk about "is the function injective and surjective"
4. Need a way to ascribe a function with these laws that it must obey.
5. Requires pattern unification a la Prolog.
5. Since `<=>` has a commutative algebra, an implementation of `a <=> b` entails one for `b <=> a`. This needs some construct.

```
trait (a: T) <=> (b: T) where
  from (a <=> b) = a
  to   (a <=> b) = b

  a <=> b
```
