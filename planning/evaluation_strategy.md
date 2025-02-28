# Evaluation strategy

There are several kinds of evaluation strategies in the context of an abstract machine, and a language can employ only one of them. This makes it important for the language to pick the correct strategy for whatever reasons they may have.

1. [Call-by-value](#call-by-value),
2. [Call-by-need](#call-by-need),
3. [Call-by-push-value](#call-by-push-value), and
4. [Call-by-coneed](#call-by-coneed).

We list them, what they are, and then we make an administrative decision on what TLTMNBN (the Language that must not be named) should use.

## Call-by-value

Most people are familiar with call-by-value. A function call is a jump to the function address with values pushed in the stack. Nothing special.

You can author code to be lazy as well by using closures and deferring its execution until it is needed, but nevertheless it is code that must be written manually to recover call-by-need semantics.

## Call-by-need

> NB: Another variation is call-by-name, which is the same as call-by-need except it recomputes the thunk for every need instead of storing the result the first time that thunk was forced.

In this evaluation strategy, a function call creates a thunk with arguments stored in, which are themselves also a thunk. When a value is used in a particular context, that thunk itself is forced, and the same rule within that thunk is recursively applied, hence the "need."

Haskell uses call-by-name and is famous for it, but is also infamous for the unfortunate interaction with `[Char]` among some other examples.

```hs
data List a = Cons a (List a) | Nil
```

Since `String` is an alias to `[Char]`, and the syntax `[a]` is defined as above, there is no cache locality. Worse, each `Cons` gives a thunk to an `a` and a thunk to the next `List a`. This problem is also furthermore not limited to lists, it also applies to `Data.Map`, `Data.Set`, and whatever have ye.

To circumvent this, library authors need to write two implementations: lazy (typical code) _and_ strict (force all the _hidden_ thunks). Most library author designs the API in such a way that lazy semantics goes in `Foo.Lazy` module and strict semantics goes in `Foo.Strict` module, preventing this footgun. If you use the wrong one, your runtime finishes in 60 seconds instead of 2 seconds.

## Call-by-push-value

[Call-by-push-value](https://en.wikipedia.org/wiki/Call-by-push-value) is relatively new and allows both of them and makes it central to the language's operational semantics. In terms of an abstract machine, the calling function propagates the computation paired with that value up the call stack hierarchy. In other words, "if someone takes this computation, this is the value they get."

But that's not the limits of call-by-push-value: it _replaces_ the need for explicit implementation of free monads and monad transformers. Furthermore it provides a way to communicate arbitrary monadic computation without being constrained by the monadic composition `x >>= f x` where a chain of functions `f :: a -> m b` must agree on one `m`.

There are a couple of programming languages using it:

1. [Eff](https://www.eff-lang.org/learn/),
2. [Effekt](https://effekt-lang.org/), and
3. [Koka](https://koka-lang.github.io/koka/doc/index.html).

The notation  has a similar notation for monads, and this makes sense. We define an `Effect` trait:

```
trait Effect e where
  sink: e a -> (a -> e' b) -> e' b
```

Indeed, that `sink` signature looks awfully identical to the `a -> m b` we saw earlier. In fact, effects are monadic. Imperative code itself, _is_ monadic!

## Call-by-coneed

Of those, call-by-coneed is very new and currently theoretical. To the best of my knowledge, there are no programming languages that uses call-by-coneed. It is built atop an idea called sequent calculus in 1935, which is decidedly not new.

Two papers on the subject:

1. [Compiling with Classical Connectives](https://arxiv.org/pdf/1907.13227).
2. [Grokking the Sequent Calculus (Functional Pearl)](https://arxiv.org/pdf/2406.14719).

At first sight, they appear identical to call-by-push-value. The main difference lies in the _soundness_ with the $\eta$-equivalence: $\lambda x.f x = f$ i.e. the $\lambda$-abstraction here can be replaced by $f$. In the paper *Compiling with Classical Connectives*, it goes on to explain that the first rule $\eta_\rightarrow$ conflicts with $\beta_{\tilde\mu}$-reduction rule, and conversely $\eta_\oplus$ conflicts with $\beta_\mu$-reduction rule in call-by-need. (hint: look at the polarities in section 2.3, and the resulting commands!)

## Proposal

We would like for the evaluation strategy in TLTMNBN (the Language that must not be named) to be call-by-star (not officially named), as in, all of it. Call-by-star has the four corners:

1. Call-by-value, for efficient data storage or strict computation.
2. Call-by-name, for lazy single-shot computation.
3. Call-by-need, for lazy single-shot computation whose values are reused.
4. Call-by-coneed, for efficient dispatching of control effects.

This would be based on the dual core calculus idea that the *Compiling with Classical Connectives* paper calls System $\mathcal{D}$ as shown in section 7.
