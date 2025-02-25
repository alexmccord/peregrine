import std.cmp.eq
import std.hash.fingerprint

trait (Eq a, Fingerprint a) => Hash a where
  hash : a -> a -> Word
  hash = fingerprint
