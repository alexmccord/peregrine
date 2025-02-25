trait Functor f where
  map : (a -> b) -> f a -> f b
