# Type system

`L`'s type system plans to support dependent types, parametric polymorphism, and functions over types. In other words, calculus of constructions, or in terms of where it falls on the lambda cube, $\lambda C$.

## Cumulative universes

Cumulativity refers to the idea that a term in some universe $U_\alpha$ is also a term in $U_{\alpha+1}$. One example is a function in $U_1$ can use some term in $U_0$ to produce a term in $U_\alpha$ for any $\alpha \le 1$, also known as dependent types. Cumulativity generalizes this even further, but not without violating Russell's paradox: any $U_\alpha$ cannot _know_ about any term in $U_{\alpha+1}$, hence the names "small" and "large" types, which `L` uses to enforce the naming convention.

In general, most of the time we live in $U_0$ (values) and $U_1$ (types) with occassional visits in $U_2$ (types of types), and beyond that is where people of certain sorts are.
