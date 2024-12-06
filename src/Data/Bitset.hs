{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Bitset
  ( Bitset,
    bitset,
    bitsetFromIntegral,
    bitsetToIntegral,
    toList,
    fromList,
  )
where

import qualified Data.BitVector.Sized as BV
import qualified Data.BitVector.Sized.Unsigned as BV
import Data.Bits
import Data.List
import GHC.IsList
import GHC.TypeLits

newtype Bitset (n :: Natural) = Bitset (BV.UnsignedBV n)
  deriving (Eq)

blift :: (BV.UnsignedBV n -> BV.UnsignedBV n) -> Bitset n -> Bitset n
blift f (Bitset bv) = Bitset (f bv)

bmap :: (BV.UnsignedBV n -> a) -> Bitset n -> a
bmap f (Bitset bv) = f bv

bliftA2 :: (BV.UnsignedBV n -> BV.UnsignedBV n -> BV.UnsignedBV n) -> Bitset n -> Bitset n -> Bitset n
bliftA2 f (Bitset l) (Bitset r) = Bitset (f l r)

bmapA2 :: (BV.UnsignedBV n -> b -> BV.UnsignedBV n) -> Bitset n -> b -> Bitset n
bmapA2 f (Bitset bv) x = Bitset $ f bv x

-- Intended to be used with type applications, e.g. bitset @2.
bitset :: forall n. Bitset n -> Bitset n
bitset = id

bitsetFromIntegral :: (Integral a, KnownNat n) => a -> Bitset n
bitsetFromIntegral = Bitset . BV.mkUnsignedBV BV.knownNat . fromIntegral

bitsetToIntegral :: (Integral a) => Bitset n -> a
bitsetToIntegral (Bitset bv) = fromInteger (BV.asUnsigned $ BV.asBV bv)

instance (KnownNat n) => IsList (Bitset n) where
  type Item (Bitset n) = Bool
  toList bs = reverse [testBit bs i | i <- [0 .. finiteBitSize bs - 1]]
  fromList xs = foldl' step zeroBits (zip [0 ..] (reverse xs))
    where
      step :: Bitset n -> (Int, Bool) -> Bitset n
      step bs (i, b) = if b then bs `setBit` i else bs

instance (KnownNat n) => Bits (Bitset n) where
  (.&.) = bliftA2 (.&.)
  (.|.) = bliftA2 (.|.)
  xor = bliftA2 xor
  complement = blift complement
  zeroBits = Bitset zeroBits
  bit = Bitset . bit
  testBit = bmap testBit
  isSigned = bmap isSigned
  popCount = bmap popCount

  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize

  shift = bmapA2 shift
  rotate = bmapA2 rotate

instance (KnownNat n) => Bounded (Bitset n) where
  minBound = zeroBits
  maxBound = oneBits

instance (KnownNat n) => FiniteBits (Bitset n) where
  finiteBitSize _ = fromIntegral (natVal @n undefined)
  countLeadingZeros = bmap countLeadingZeros
  countTrailingZeros = bmap countTrailingZeros
