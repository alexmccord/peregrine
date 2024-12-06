module Language.MorLam.IR () where

-- A category is composed of 4 different things.
-- `Ob`, a class of objects.
-- `Mor`, a class of morphisms between `Ob`s.
-- `id`, an identity morphism.
-- `(.)`, a composition of two morphisms.
--
-- To be a _valid_ category, the composition
-- operator must satisfy three laws:
-- 1. associativity: (h . g) . f = h . (g . f)
-- 2. left identity: id . f = f
-- 3. right identity: f . id = f
--
-- Any instances of a category may impose further
-- constraints any way they wish, but they cannot
-- be more lax than the category they belong to.
