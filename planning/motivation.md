# Motivation

It comes from my never-ending thirst for the ideal programming language that hits just the sweet spot for my ADHD brain, without having to sacrifice the design I wanted to go for in order to satiate my OCD brain.

The first language I picked up on was Lua in the context of Roblox. It was a simple language, and obviously it is not without its flaws, but the amount of footguns in the language is actually pretty small.

The next thing I did was pick up on C# and TypeScript not long after it. This exposed me to a whole new level of development toolings I did not know about: autocomplete guided by its type system, along with the ability to detect logical errors long before you run the program.

After that, going back to Lua was pretty painful, knowing that I no longer had the tools I've come to depend on to accelerate development. This was what motivated me to build [Lunar](https://github.com/lunarlang/lunar) which was eventually subsumed when Roblox hired me to work on [Luau](https://github.com/luau-lang/luau).

I remember seeing people claiming that you should not build a programming language. For those people, I would like to tell them to fuck right off with such unfounded prejudices. What they are doing is silencing the personality trait that someone might have within them, which is one of the worst thing one can do to someone. I was told this several times at RDC 2019, and each time felt like an attack on my personality. Nevertheless, I ignored them and persevered because I believed in it. At the end, I was rewarded with an internship and not long after it, a full time offer at Roblox. Again, to those people: go fuck yourselves. But I digress.

During my time at Roblox, I had to learn C++ in order to work on Luau. Couple this with the knowledge of how much nicer other languages have it with deriving `Eq` or `Hash` and such to reduce boilerplate code so you can focus on the actual domain specific problem. I would be lying if I said I wasn't contemplating a language to kill C and C++ once and for all.

With time, I came to the conclusion that I'm not the sort of person who is willing to accept the status quo and getting on with life. My OCD pushes my engineering brain to do something about it, lest I risk paronychia from my instinctual OCD behaviors like nail picking and biting.

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
