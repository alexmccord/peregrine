# `L`

> NB: Since the language doesn't even yet have a name, it will be designated as `L`.
>
> I have thought about calling it `SC`, as in a "successor of C" using the successor function notation. That name is pretty cute compared to things like C++, C#, D, C2, C3, etc. But phonetically speaking, it has multiple syllables for all variations: "SC" or "succ of C" or "successor of C" for which none flows smoothly, which is a drag.
>
> Nevertheless, the list of candidate names:
> 1. SC
> 2. Clu

`L` aims to be a research project (read: theorycrafting) that hopes to be an assemblage of some advancements in type theory and programming language designs of the past several decades.

In particular, it plans to be a memory safe, FFI safe, dependently typed functional programming language usable all the way down to systems programming.

> NB: These are things I _hope_ to have, but it does not mean I have figured out a way for all of them. Particularly memory safety and FFI safety are the biggest questions.
>
> **Do not take them as a proclamation**. The implementation doesn't even exist or is incomplete!

**Memory safety**. Like Rust, if `L` builds, there shall be no erroneous use of memory. What remains to be seen is the approach this entails.

| Strategy   | Stance | Reason
|------------|--------|--------
| Something? | ?      | Research project! Literally anything!
| Typestates | ?      | Essentially a superset of borrowck if included.
| Borrowck   | Maybe? | Good approach, but has "fighting the borrow checker."
| ARC        | No.    | Unacceptable tradeoffs for systems programming.
| GC         | No.    | Not usable in systems programming.

**FFI safety**. Most bindings calling into FFI calls have problems because it requires the maintainer to take into account a huge set of problems, and relies on past experiences of authoring any FFI bindings to improve correctness.

> NB: this isn't intended to call out on the abilities of such maintainers. Simply that all FFI bindings are fundamentally walking on eggs. I have great many respect for maintainers of FFI bindings.

**Dependently typed**. They're great at enforcing invariants. We will be using them.

For termination guarantees, we put in a restriction where functions can only be applied on terms that has a well-founded partial order.

**Systems programming**. This means no runtime and no garbage collector. Just the set of primitives and syscalls upon which to build the universe.

## Roadmap

1. Implement up to Turing completeness.
2. Bootstrap.
3. Implement the rest.
