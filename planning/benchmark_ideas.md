# Benchmarks

Some references:

1. https://en.wikipedia.org/wiki/The_Computer_Language_Benchmarks_Game

## Ideas

Just a couple of ideas.

1. This one is to test for memory buffer reuse when cycling between `Just` and `Nothing`.
```hs
collectCollatz : Natural -> Maybe [Natural] -> Maybe [Natural]
collectCollatz n
  | n <= 1    = id
  | even n    = collectCollatz (n / 2) . (liftA2 (::) n)
  | otherwise = collectCollatz (n * 3 + 1) Nothing
```
