# Standard library

The guiding principles of the standard library:

**Learn and avoid mistakes from other standard libraries**. Motivated by Haskell. Haskell has a function `fmap` separate from `map` that works for `[a]` whereas `fmap` works for any instances of `Functor`, _including_ `[a]`. This separation exists because changing `map` to be general, while technically behavior preserving at runtime, would break type inference since it can't figure out which `data` a term has.

Obviously this is not to say that the standard library we will build is flawless, as much as one wishes it to be so. Hello, Murphy's law. No, it just gives rise to the second point.

**Be willing to sacrifice backward compatibility for the greater good**. If there exists a counterexample to the hierarchy, be willing to fix it. In my opinion, it's hard to build a cohesive mental model of Haskell's functions and which one to use is specifically due to "all these functions does the same thing but their signature are different." Another example is `pure` and `return`, which categorically must always be equivalent. This results in a problem of requiring tribal knowledge, and anywhere with tribal knowledge results in a poor onboarding experience.

**Redesign anything caused by other languages' limitations**. Motivated by my ADHD brain. Imagine you wrote code that you knew were bijective but there was nothing you can do to avoid writing the inverse manually, which leaves a sour taste.

---

There will eventually be a set of modules for practical programming, but it doesn't exist yet.

Since the language builds atop the operational semantics and is intrinsically tied to the language, the standard library will be divided into tiers in order to allow granular control over the level of abstractions.

> NB: it remains to be seen if that's the case.

| Tier | Module
|------|--------
| 0    | `OpSem`

## Operational semantics

Operational semantics defines the computational model of the programming language along with the IR types.

## Category

## Number systems

This is where you'll find your numbers.

1. `Natural`,
2. `Integer`,
3. `Ratio a b`,
4. `Rational`,
5. `Real`,
6. `Complex`.
