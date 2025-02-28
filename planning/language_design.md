# Language design

## Syntax

Reminder that EBNF is incapable of expressing the actual language perfectly, but it's a nice medium as a starting point.

> A module system needs to be designed which will make modifications to the syntax.

```ebnf
prog ::= {stmt};

decl ::= `let`
       | `struct`
       | `data`
       ;

equation ::= expr `=` expr;
eqfamily ::= {expr} (* yes, expr instead of equation *)
stmt ::= decl equation
       | decl expr `where` eqfamily
       ;

expr ::= expr {expr}   (* application *)
       | `(` expr `)`  (* association *)
       | atom          (* boring      *)
       ;

atom ::= ident
       | string
       | number
       | operator
       ;

(* The rest are the boring stuff *)
alpha ::= `a`...`z` | `A`...`Z`; (* yawn *)
digit ::= `0`...`9`;
ident ::= alpha {alpha | digit | `_` | `'`};

base_bin ::= `0`...`1`;
base_oct ::= `0`...`7`;
base_dec ::= `0`...`9`;
base_hex ::= `0`...`9` | `a`...`f` | `A`...`F`;

(* This stuff is extremely boring, I decided to move on *)
(* Until a formal spec is required, apply intuition     *)
```

> Reminder to self: there's a good reason for `where` keyword being there. If you add `let` expressions in the body, then forgetting to indent the `let` expression causes it to bind to the wrong scope.

As you can see, the syntax is very small. This is the consequence of the following:

1. dependent types,
2. expression/pattern uniformity, and
3. syntactic composition.

By "expression/pattern uniformity," we mean there is no difference between the intensional property, only just by their extensional property. How this can be done is by treating expressions whose unbound variables in a positive polarity as being isomorphic to the $\forall$ quantifier.

Using Rust as an example, the intensional part is the function signature and the extensional part is the body of the function.

```rs
fn add(x: i32, y: i32) -> i32 { x + y }
```

| Rust context     | Intensional                  | Extensional
|------------------|------------------------------|-------------
| `fn` declaration | `add(x: i32, y: i32) -> i32` | `x + y`
| `fn` application | N/a                          | `add(1, 2)`

With the uniformity in TLTMNBN (the Language that must not be named), the only difference that remains are that intensional and extensional only tell us _what_ to do with unbound variables. As an example:

```
trait x + y where
--    ^^^^^
--      | intensional
  Add x y = x + y
--^^^^^^^   ^^^^^
--   |        | extensional
--   | intensional
```

Which tabulates like so:

| TLTMNBN (the Language that must not be named) context | Intensional | Extensional
|-------------|-------------|-------------
| expression  | `x + y`     | `(+): (x: Type) -> (y: Type) -> Trait`
| expression  | `Add x y`   | `x + y`

## Ideas

If one writes a bunch of equalities, and it worked like it did in Haskell where each equation is tested one by one, it can be too easy to write an equation that would be subsumed by a prior one, causing unintended results.

One option to resolve this is to expose specificity of each equation in a sequence of patterns. One could then use that information to lint (or enforce?) saying that you need to reorder the equations such that the one with the greatest specificity goes first, and all other equations belonging to that same specificity class must be placed together.

You can also use this information to rearrange the order of equations that would get evaluated, but this raises a cause of concern. I'd rather a lint or an error.
