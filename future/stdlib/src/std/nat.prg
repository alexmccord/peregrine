import std.bool

data Natural =
  | Zero
  | Succ Natural

NatN : Natural -> Type
NatN width = forall n : Natural | n < (2 ^ width). n

Nat8 : Type
Nat8 = NatN 8

Nat16 : Type
Nat16 = NatN 16

Nat32 : Type
Nat32 = NatN 32

Nat64 : Type
Nat64 = NatN 64

(+) : Natural -> Natural -> Natural
Zero   + n = n
Succ m + n = Succ (m + n)

(*) : Natural -> Natural -> Natural
m * Zero   = Zero
m * Succ n = m + (m * n)

(^) : Natural -> Natural -> Natural
m ^ Zero   = Succ Zero
m ^ Succ n = m * (m ^ n)

(<) : Natural -> Natural -> Bool
Zero   < Zero   = False
Zero   < Succ _ = True
Succ _ < Zero   = False
Succ m < Succ n = m < n
