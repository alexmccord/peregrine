# Language Design

## Syntax

Reminder that EBNF is incapable of expressing the actual language perfectly, but it's a nice medium as a starting point.

> A module system needs to be designed which will make modifications to the syntax.

```ebnf
prog ::= {stmt};

decl ::= `let`
       | `trait`
       | `impl`
       | `struct`
       | `enum`
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

By "expression/pattern uniformity," we mean there is no difference between the intensional property, only just by their extensional property. How this can be done is by treating expressions whose unbound variables in a positive polarity are isomorphic to the $\forall$ quantifier.

Using Rust as an example, the intensional part is the function signature and the extensional part is the body of the function.

```rs
fn add(x: i32, y: i32) -> i32 { x + y }
```

| Rust context     | Intensional                  | Extensional |
|------------------|------------------------------|-------------|
| `fn` declaration | `add(x: i32, y: i32) -> i32` | `x + y`     |
| `fn` application | N/a                          | `add(1, 2)` |

With the uniformity in `L`, the only difference that remains are that intensional and extensional only tell us _what_ to do with unbound variables. As an example:

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

| `L` context | Intensional | Extensional             |
|-------------|-------------|-------------------------|
| expression  | `x + y`     | `(x: Type) + (y: Type)` |
| expression  | `Add x y`   | `x + y`                 |
