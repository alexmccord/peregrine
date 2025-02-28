import std.ctrl.monad

trait Applicative m => Monad m where
  (>>=) : m a -> (a -> m b) -> m b
