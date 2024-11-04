# Naming convention

For each universe, a term is the "small" part and a type is the "large" part.

1. Use snake_case for terms.
2. Use PascalCase for types of terms.

The language enforces an identifier in the "small" part of the universe to be in the lowercase, and the "large" part in the uppercase.

## Universes

Universes are inductively defined:

$$
V_{o}S = \begin{cases}
S                                   &\text{if } o = 0 \\
S \cup P(S)                         &\text{if } o = 1 \\
\displaystyle\bigcup_{i=0}^o V_{i}S &\text{otherwise}
\end{cases}
$$
