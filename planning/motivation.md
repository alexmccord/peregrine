# Motivation

> TL;DR: I'm limited by the technology of my time.

## What drove me this time?

I wanted to build a game engine running as close to the metal as possible. This leaves me with a couple of options: C, C++ or Rust. This got me thinking, are we really that starved?

1. I automatically excluded C because operator overloading and templates are must-haves.
2. C++ is essentially C and Rust. Operator overloading, templates, and manual memory management.
3. Rust's approach to memory safety causes development iteration unbearably slow in the context of game engine development.

I ended up going with C++, and after setting up the initial goop such as writing CMake, linking a bunch of source files to a project, and adding dependencies such as Vulkan, glfw, and glm, and I was good to go.

The next problem I ran into is the nature of Vulkan's API vs imperative code. For instance, there's an enum called `VkResult` which can be partitioned into three parts:

1. Zero for `VK_SUCCESS`,
2. Positive nonzero for successful operations with information pertaining it.
3. Negative nonzero for erroneous operations with information pertaining it.

The way one "makes their lives simpler" is by defining a macro:

```c
#define VK_CHECK(e)       \
    do {                  \
        VkResult res = e; \
        if (res < 0)      \
            return res;   \
    } while (false)
```

Which is familiar to Haskellers as akin to a monad equipped with `MonadFail` where computation continues if and only if the condition `res >= 0` holds. This is a good programming technique, your code is not littered with handling each failure line by line, they get bubbled up to some handler to sink the error. The problem lies in the fact that one must call `VK_CHECK` each time. I missed a couple!

## Problems

### Systems languages

Your options for a systems language is pretty small.

1. C
2. C++
3. Rust

The ones that be upcoming are as follows:

1. Zig
2. Nim

And that's it.

## C

## C++

See [the rant](./the_rant.md). In short, C++ is essentially a chimera that belongs to an ancient civilization long past its expiration date.

## JavaScript



## Haskell

I think it's a fantastic language itself. It has proven that functional programming is probably the way to go provided that the compiler can produce an equivalent optimal imperative code behind the scene.

My main qualms are as follows:

1. The module system feels like it was cobbled together quickly.
2. The _culture_ surrounding the idea of composing functions together.
