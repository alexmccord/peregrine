# Build systems

After dealing with some frustration caused by various build systems or version management e.g. `ghcup`, I have decided that the build system for `L` must be hermetic.

The principle here we want to follow is Newton's first law, i.e. the final output remains identical given some state unless any force was applied onto the system. Thus, if you were to `git clone` a repo, there must be a guarantee that it's always safe to execute the build script, and a guarantee that it will produce the same exact binary for a target machine.

Some great role models for our build system we'd like to follow:

1. https://nixos.org/
2. https://github.com/bazelbuild/starlark

## Nomenclature

The name [module](./module.md) are already used over, and using the plural form will lead to confusions. In `L`, the name for a collection of modules are called _apparatus_.

An apparatus can depend on another apparatus, and only an apparatus.

> NB: whether this name will stick remains to be seen.

## Proposal

In order to have the build system be reproducible on every machine regardless of the machine state, we need to ensure hermeticity. If achieved, this prevents global side effects on the machine, isolates the project's dependencies from the machine, and everyone can freely clone the apparatus and build it.

To do this, there needs to be some way to generalize the dispatch logic that happens in the `IO` type. In Haskell, their definition of `IO` is:

```hs
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

The way to get around this problem is by using a free monad, which gives you a way to encapsulate all the computation essentially in the form of `Monad m => MyFreeMonad (m a)`. This approach requires authoring a slew of code to lift into [basically control effects](https://en.wikipedia.org/wiki/Call-by-push-value) with extra steps.

To that end, we would need to introduce the notion of an effect system for which we could implement our virtual IO.

```
trait Sink a where
  sink: a -> b
```

> NB: we would like to avoid the "so long as these constraints are upheld by the programmer" problem too: not just the author of the build script but also the actual virtual IO implementation.

The `IO` type is thus redefined to be a collection of effects that it's allowed to have:

```
enum OSError where
  file_does_not_exist
  ...

let IOResult a = Result a OSError

enum IO where
  read_file Path
  write_file Path String
  exit ExitCode
  ...

trait IO where
  read_file: Path -> IOResult String
  write_file: Path -> String -> IOResult ()
  exit: ExitCode -> IO.exit

impl Sink IO where
  -- IO's exit is final, forcing upward propagation.
  sink exit e = exit e
  -- the rest are left for the context to bind custom sink impls.
```

And `main` has a _default_ sink function over all the effects in `IO`, and we do the same to `build` likewise, with a different implementation, as defined by `SystemIO` and `VirtualIO`.

The build script is siloed off from the apparatus, so the build script cannot depend on things in it, nor can the apparatus depend on things from the build script. This also has the nice effect of making it safe to `git clone` some random project written in `L`.

Each apparatus can have one and only one `build.l` file, and it must live at the directory root of the apparatus. The entry point for it will deviate slightly.

```
let build: ?? = do
  ...
```

<!-- While the signature `IO ()` implies that `build` will have side effects on the machine, this will not be the case: the compiler will lift a function representing the virtual world into `IO` and bind it to `build`, making it a _virtual_ IO.

As a result, the interface is exactly identical, the only difference is all effects must go through this virutal IO, which decides whether any filesystem access is allowed or not, depending on if it attempts to escape the sandbox. -->

The build system will also create a folder to persist any mutable state, which can be used by the apparatus that it builds for, and only for that one.
