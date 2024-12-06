module Category where
  Ob  :: Type
  Mor :: Ob -> Ob -> Type
  id  :: Mor a a
  (.) :: Mor b c -> Mor a b -> Mor a c

  law associativity  :: forall h g f. h . (g . f) = (h . g) . f
  law left-identity  :: forall f. id . f = f
  law right-identity :: forall f. f . id = f
