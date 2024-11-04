# Isomorphism classes

There exists an isomorphism between $a$ and $b$ iff there is an injective function $f : a \to b$ and its inverse $f^{-1} : b \to a$.

In most languages that doesn't check for totality will not be able to guarantee it. Of those that does, does not really lean on it, as far as I can tell.

In `L`, I plan on using isomorphism classes as an analogy to translating types in `L` onto the ideal IR type. The keyword here is "ideal" which is compiler defined.

## In practical use

For instance, you can define the `Nat` type to be isomorphic to some `OpSem` type representing that value.

```
enum Nat where
  Z
  S Nat

impl Nat + Nat where
  x + Z   = x
  x + S y = S x + y

impl Nat <=> OpSem.UInt32 where
  Z   <=> 0
  S n <=> 1 + n
```

Then the compiler can select the ideal runtime representation within the isomorphism class if it was so compelled to.

Since it is an isomorphism _class_, you can also have `Nat` be isomorphic to arbitrarily many others, as long as there is a proof that they are isomorphic. The onus is on the compiler to verify that using operational semantics.

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
