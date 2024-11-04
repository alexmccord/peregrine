# Parametricity and composition

**Nuance**: I understand that C++ doesn't necessarily prioritize programming elegance. My gripe is is the laborious tasks involved whenever the program complexity has to increase and code written in program state $p$ is unaware of new requirements in program state $p'$, resulting in manual traversal over the source code. Since this manual traversal is done by a human, there could always be some function that did not account for some new state.

Nevertheless, I am deeply unhappy with the state of most programming languages from antiquity refusing modern advancements in programming language design _even if_ there exists a path where a feature can be retrofitted without ruining the identity of the language. Looking at you, Java (`@FunctionalInterface`, really?) and Go (took a while to add generics).

I'd rather spend my efforts on repositioning the state of existing programming languages, lest them be wasted on wallowing in defeat.

## Prefacing

As you read the rant, you might be tempted to mention that C++ has since added `std::invoke` which attempts to generalize all this problem away. That's nice, but my problem with this approach are twofold:

1. there are still many corner cases outside of function applications.
2. this is a _standard library feature_ as opposed to a _language feature_.

That second point is a common trope in the philosophy of C++'s design, there are many others like it: `std::variant`, `std::function`, and so on.

Typically, if a feature is added as part of the standard library, it ends up being orthogonal to other language features _by default_. This means the standard library and the language features can be misaligned, and keeping things in sync is boring grunt work.

Furthermore, since `std::variant` is not a first-class language feature, it is not possible to author any analysis deciding whether all alternatives in the variant has been matched. There is `std::visit` which helps, but again, this ended up as a standard library feature. It was [_almost_](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0095r1.html) a first class feature, even supporting polymorphic variants as seen in OCaml.

Anyway, onto the _next_ rant:

## The Rant

Pardon the $\LaTeX$ soup. I discovered it recently and decided to abuse notation to distract myself from the horror show.

C++ considers the following types of function calls to be orthogonal:

```cpp
f(x);
x.f();
```

Here, C++ treats `f` as a function of arity 1, whereas `g` is treated as a function of arity 0 with extra steps: a function of arity 1 ascribed by a notion of `g` being a _member function_ of `X`.

| $A$ | Application       |
|-----|-------------------|
| $P$ | `f(args...)`      |
| $Q$ | `(t.*f)(args...)` |

Consequently, you can't write a generic `apply` function over $P$ and $Q$ simply as:

```cpp
template<typename F, typename... Args>
auto apply(F f, Args&&... args)
{
    return f(std::forward<Args>(args)...);
}
```

For instance, you can call `apply(f, x)`, or equivalently, `apply(&f, x)`, but member functions presents a counterexample, `apply(&X::f, x)`. The resolution there is _even more code_ to resolve the composition deficiency in the language:

```cpp
template<typename F, typename T, typename... Args>
auto apply(F f, T&& t, Args&&... args)
{
    return (t.*f)(std::forward<Args>(args)...);
}
```

Wonderful. Now we can say `apply(&X::f, x)`, but of course we run into the next issue: `apply(f, x)` and `apply(&f, x)` now resolves to the second overload here because `apply(F, T&&, Args&&)` has a greater specificity than `apply(F, Args&&)` which causes overload resolution to select our new `apply` overload. We need to SFINAE, so we add `decltype(expr)`s:

```cpp
template<typename F, typename... Args>
auto apply(F f, Args&&... args) -> decltype(f(std::forward<Args>(args)...))
{
    return f(std::forward<Args>(args)...);
}

template<typename F, typename T, typename... Args>
auto apply(F f, T&& t, Args&&... args) -> decltype((t.*f)(std::forward<Args>(args)...))
{
    return (t.*f)(std::forward<Args>(args)...);
}
```

> *Tangent*: adding an overload has the capability to re-target all existing function calls onto an entirely different control flow path. In this instance, we got lucky with a compile error. Other times we might not be as lucky.

$$
\begin{aligned}
apply_P(f,x_1,\dotsc,x_n)       &= f(x_1,\dotsc,x_n)\\
apply_Q(f,t : T,x_1,\dotsc,x_n) &= f_T(t, x_1,\dotsc,x_n)\\
\end{aligned}
$$

Great. Unfortunately, we find a new way to be disappointed. C++ _also_ treats the following to be orthogonal:

```cpp
(x.*f)();
(x->*f)();
```

We need to break $Q$ into two new independent variables, $Q'$ (renamed as $Q$ going forward) and $R$:

$$
\begin{aligned}
apply_P(f,x_1,\dotsc,x_n)        &= f(x_1,\dotsc,x_n)\\
apply_Q(f,t : T,x_1,\dotsc,x_n)  &= f_T(t, x_1,\dotsc,x_n)\\
apply_R(f,t : T*,x_1,\dotsc,x_n) &= apply_Q(f,*t,x_1,\dotsc,x_n)\\
\end{aligned}
$$

You would need another overload.

```cpp
template<typename F, typename T, typename... Args>
auto apply(F f, T&& t, Args&&... args) -> decltype(apply(f, *t, std::forward<Args>(args)...))
{
    return apply(f, *t, std::forward<Args>(args)...);
}
```

> *Tangent*: by the way, since `&f` and `f` are utterable, then surely `&X::f` and `X::f` are also utterables? For some reason, the former is and the latter isn't.

> *Tangent*: there is no resolution in the case of passing an overloaded function as a higher order function! As soon as overloaded functions enter the picture into higher order logic, everything falls apart.

We distribute over $A = \set{P,Q,R}$ the positive polarity of function applications. I assert:

$$
\frac{
    \Gamma \vdash (P \lor Q \lor R)
    \qquad
    \Gamma \vdash P \neq Q
    \qquad
    \Gamma \vdash Q \neq R
}{
    \Gamma \vdash (P \land \neg{Q} \land \neg{R}) \lor (\neg{P} \land Q \land \neg{R}) \lor (\neg{P} \land \neg{Q} \land R)
}
$$

If $P = Q$ and $Q = R$ were to hold:

| $A$ | Application  |
|-----|--------------|
| $P$ | `f(args...)` |
| $Q$ | `f(args...)` |
| $R$ | `f(args...)` |

And because we have uniformity $P = Q = R$, the language ends up with a parametrically polymorphic function $apply$. One could perhaps call this "universal function call syntax." All that has to be written is as follows:

$$
\begin{aligned}
apply(f,x_1,\dotsc,x_n) = f(x_1,\dotsc,x_n)
\end{aligned}
$$

/endrant.
