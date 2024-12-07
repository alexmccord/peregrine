# Records in Peregrine

The concept of an "extensible record" varies on a per language basis, depending on their paradigm or the constraints that a language has to abide by, due to
their specific type system. Some of it has been listed below.

[Something to read for later](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf).

## The problem

Suppose you have a problem where you'd like to model a whole bunch of data:

```hs
data MyRecord = MyRecord
  { foo :: Foo
  , bar :: Bar
  , baz :: Baz
  }
```

It's not a bad start. Next, you want to make it open for extension, so you offer a way to allow the user to clip on their data into your record. The first thing you might write would be:

```hs
data MyRecord u = MyRecord
  { foo :: Foo
  , bar :: Bar
  , baz :: Baz
  , userdata :: u
  }
```

It's okay, but you notice that the user has to get their userdata by going through the `userdata` field explicitly. That's unfortunate, but such is life.

In some cases, however, what you actually wanted is a way to inscribe only one copy of that specific part of data to any given node, and it's not necessarily the case that _all_ data has been inscribed. This is the reality of ECS or other data modeling problems like it.

```hs
data MyRecord u = MyRecord
  { foo :: Maybe Foo
  , bar :: Maybe Bar
  , baz :: Maybe Baz
  , userdata :: u
  }
```

At this point, you probably flip your table inside out, so you make a typeclass, along with operations to be able to retrieve it.

```hs
class Component c
```

Then you realize: for each node, you are paying for data that doesn't exist yet. This memory usage is unfortunate, so you'd

```
data Table xs = Table
```

### In dynamic languages

In a dynamic language like Python, JavaScript, and Lua, records are inherently extensible.

### In gradual type systems

Records in gradual type systems varies a little bit.

Records are structural, forced by the constraints of the language they try to type.

Also includes set theoretic operators such as `&` and `|` to contract or expand the domain of terms, and for records.

```
function hypot(p: { x: number, y: number })
  return math.sqrt(x^2 + y^2)
end
```

### In C

For instance, in C, the concept of an extensible record is usually done with two fields. The way this is done in Vulkan is:

```c
typedef enum VkStructureType {
  VK_STUFF_STRUCTURE_TYPE,
  // ... 1109 lines later ...
  VK_STRUCTURE_TYPE_MAX_ENUM,
} VkStructureType;

typedef struct VkStuff {
  VkStructureType sType;
  const void* pNext;
  // ... stuff ...
} VkStuff;
```

Where `VkStructureType` is just a custom RTTI telling us which structure this pointer is, and is then cast into it using `(ThatStructure*)data`. This is just an alternative way of using `union` in C anyway, only that it gives a way to be ABI-compatible if one wishes to add a new structure type causing the data layout to be larger than what it was.

### In C++

This is usually done with class inheritance, but there are other options too if inheritance is to be avoided. Generally though, the C approach is no longer done in this language since C++ has templates.

### In Haskell



### In Elm

Elm did something a bit nice for this, but is intended to support anonymous records since they live atop JavaScript runtime.

## Records in Peregrine

Haskell _technically_ supports extensible records, but it doesn't seem founded in set theoretic ideas of an intersection type. Suppose you have your records

```
record Foo where
  x: Nat
  y: Bool
```

But you want to provide a way for users to attach additional data alongside your record. In most languages, this results in tedious amount of work, overloading the language's field lookup logic, let alone support the notion that a record may have duplicate key name, because perhaps someone else wishes for their record to hitchhike with yours:

```
record Bar where
  y: String
  z: Nat
```

Then the resulting `Foo & Bar` gives the following layout:

```
record Foo & Bar where
  x: Nat
  y: Bool   -- !!
  y: String -- !!
  y: Nat
```

Notably though, if the type system can work bidirectionally (which it should, obviously, it would be lame otherwise), then the type system can select whichever field to use despite the duplication. Rust does this. Haskell too, in select cases.

This kind of stuff comes up from time to time. Imagine this:

```
record Core where
  loc: SourceLoc
  ...

struct Analysis e s where
  errors: Vec e
  analysis: Core & s

let run_analysis: [Rule] -> Analysis e s -> Analysis e s = do
  ...

let add_data: s -> Analysis e s -> Analysis e s = do
  ...
```

Or you know. ECS.

```
record Health where
  hp: Rational

record Transform where
  position: Vec3

record Velocity where
  a: Vec3

record Player = Health & Transform & Velocity
```
