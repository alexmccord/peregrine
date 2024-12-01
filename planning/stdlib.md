# Standard library

The language is defined in terms of its operational semantics, which follows that the standard library is too.



There will eventually be a set of modules for practical programming, but it doesn't exist yet.

In order to support any one of these:


Since the language builds atop the operational semantics and is intrinsically tied to the language, the standard library will be divided into tiers in order to allow granular control over the level of abstractions.

| Tier | Module
|------|--------
| 0    | `OpSem`

> NB: only a couple of things will be listed, but is not set in stone nor limited to.

## Operational semantics

Operational semantics defines the computational model of the programming language along with the IR types.

## Category

```
-- if `a <=> b` is implemented, try to derive other similar truths
trait (a: T) <=> (b: T) where
  -- TODO: from and to is ambiguous about its ordering
  -- e.g. from a to b, or to a from b.
  from (a <=> b) = b
  to   (a <=> b) = a
  a <=> b
```

## Higher algebra

```
let O (n: Nat) =
  | n == 1 = a
  | n > 1  = a -> O (n - 1)

-- trait O (n: Nat) =
--     | n == 1 = (a: Type)
--     | n > 1  = (a: Type) -> O (n - 1)
--     where
--       id: a
--       op: a -> O (n - 1)

-- trait Operad n = O n where -- says Operad n = O n _plus_ the following in `where`:
--     compose f (op: O m)
```

## Number systems

This is where you'll find your numbers.

1. `Natural`,
2. `Integer`,
3. `Ratio a b`,
4. `Rational`,
5. `Real`,
6. `Complex`.
