import std.ctrl.functor

trait Functor f => Applicative f where
  pure  : a -> f a
  (<*>) : f (a -> b) -> f a -> f b
