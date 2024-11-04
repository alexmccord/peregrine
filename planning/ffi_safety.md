# FFI Safety

> NB: This document will be worked on later, since FFI and ABI are finicky beasts, and it's better to work on this when I am in the weeds of it. Nevertheless, it is an explicit goal of `L` to _try_ to be FFI safe.

TODO: Elaborate on how the paper is used to help improve FFI safety in `L`.

1. [Semantic Encapsulation using Linking Types](https://www.khoury.northeastern.edu/home/amal/papers/lt.pdf).

## Motivation

1. You must pay for the overhead of marshalling in either directions.
2. You must know the invariants that has to be upheld.
3. You must write code to assert the invariants are upheld.
4. You must write tests to ensure invariants are not broken between versions.
5. You must be aware of FFI gotchas.
6. So many more, such as ABI versioning, idiomatic wrappers, etc.

As an example, let's say you have some hypothetical C library `libgeometry`, and you wanted to let people use a datatype from there, there's a fair amount of work involved.

## Isomorphisms

Suppose in C you have the following:

```c
typedef enum {
    GEOMETRY_CIRCLE,
    GEOMETRY_TRIANGLE,
} GeometryShapeStructureType;

typedef struct {
    float r;
} GeometryCircle;

typedef struct {
    float b;
    float h;
} GeometryTriangle;

typedef struct {
    GeometryShapeStructureType type;
    union {
        GeometryCircle circle;
        GeometryTriangle triangle;
    };
} GeometryShape;
```

And we want to take the above and transform it into our idiomatic variation:

```
enum Shape where
  Circle Radius
  Triangle Real Real
```

Then one would write the following isomorphisms:

```
impl Shape <=> GeometryShape where
  Circle   <=> GEOMETRY_CIRCLE circle
  Triangle <=> GEOMETRY_TRIANGLE triangle

impl Shape <=> GeometryCircle where
  Circle r <=> r

impl Shape <=> GeometryTriangle where
  Triangle b h <=> b h
```
