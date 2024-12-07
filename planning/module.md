# Modules

The design here is still being worked out.

TODO: read https://graydon2.dreamwidth.org/253769.html and research on first-class modules and all the goodies from over yonder.

Nevertheless, there may or may not be some modifications that I want to make from the usual ML design of modules, possibly.

## Peregrine

> I wrote down my philosophical opinions on what the module system should be, but I have yet to read up on first class modules, so expect this to be changed.

Some philosophy to consider:

1. The module dependency tree must be a DAG.
2. A file is a unit.
3. The name of the file is the name of the unit.
4. The filesystem path does not name the path to the unit.
5. If a collection of units is nested under a folder, require a "module interface."
6. The module interface is nested under that folder.
7. The module interface name must be in agreement with the name of the folder it nests.

I think I'd much rather have the source code also be the module interface as well, but what the design will be, will be decided later.

## Rust

Rust's module system is pretty okay, but a few small changes would make it even better.

For instance, it has a problem where you have many `lib.rs` or `mod.rs` in a project, which doesn't give any indication about whose crate they belong to.

It also has to account for several ways to define the crate's exports:

1. If `./foo.rs` is a sibling of `./foo/*.rs`, then `foo.rs` is that crate's exports.
2. If `./foo/mod.rs` exists, then `mod.rs` is that crate's exports.
3. If `./foo/lib.rs` exists, then `lib.rs` is that crate's exports, and that crate is a library.

And the compiler has to issue an error if more than one of these conditions are met.

Lastly, another issue I often run into are orphaned `*.rs` files which just occurs as a result of missing a `mod` declaration linking it to the crate.

## Haskell

Haskell's module system seems to have some mutual exclusion rules.

1. `module M where`: exports _all_ symbols within the module.
2. `module M (f, g) where`: exports just `f` and `g` from the module.

If you write a module and went with #1 and after a while decide to restrict the interface, you change the declaration to #2. Now all existing symbols that you did intend to export must now be added into the list of exports manually. Pain.

One other issue, albeit I'm not sure if this is due to the module system or name resolution (probably this one), but suppose you have:

```hs
import Data.Map
import Data.Set
```

and you type `fromList xs`, Haskell doesn't (and can't) resolve it to `Data.Set.fromList xs`. Instead of adding the fully qualified path, you you go and change it to:

```hs
import Data.Map
import qualified Data.Set as S
```

And you go update it to `S.fromList xs`, but also now `Set a` now must be written as `S.Set a`, which leaves a sour taste so you adjust once more:

```hs
import Data.Map
import Data.Set (Set) hiding (..)
import qualified Data.Set as S
```

Then you have a realization: `fromList :: [a] -> Map a` v.s. `S.fromList :: [a] -> Set a`. So you adjust it once more:

```hs
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
```

And with the code fixed up to be `M.` or `S.` qualified, you make a promise to never `import Mod` again.

I don't think the resolution is necessarily to add C++'s ADL to Haskell, but this kind of import system leaves a bad taste with its interaction with name resolution.

## OCaml

In OCaml, you have `.mli` which defines the set of symbols that are visible as an interface into its corresponding `.ml`. This is not bad, but it reeks of the same problems I run into with `.h` and `.cpp` files.
