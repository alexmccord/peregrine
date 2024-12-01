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

import Data.Bits
import Data.Bool
import Data.Fin
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Type.Nat hiding (Nat (..))
import Data.Vec.Pull (Vec)
import qualified Data.Vec.Pull as Vec
import GHC.IsList
import GHC.TypeLits

newtype Bitset (n :: Natural) = Bitset (Vec (FromGHC n) Bool)

-- Intended to be used with type applications, e.g. bitset @2.
bitset :: forall n. Bitset n -> Bitset n
bitset = id

bitsetFromIntegral :: forall n a. (Bits a, SNatI (FromGHC n)) => a -> Bitset n
bitsetFromIntegral x = fromList [testBit x i | i <- [0 .. Vec.length @(FromGHC n) undefined - 1]]

bitsetToIntegral :: forall n a. (Bits a, SNatI (FromGHC n)) => Bitset n -> a
bitsetToIntegral (Bitset xs) = Vec.foldl' f zeroBits (Vec.zipWith (,) Vec.universe xs)
  where
    f :: a -> (Fin (FromGHC n), Bool) -> a
    f acc (i, True) = setBit acc (fromIntegral $ toNat i)
    f acc (_, False) = acc

instance (SNatI (FromGHC n)) => Eq (Bitset n) where
  Bitset xs == Bitset ys = xs == ys

instance (SNatI (FromGHC n)) => IsList (Bitset n) where
  type Item (Bitset n) = Bool
  fromList xs = Seq.foldlWithIndex f zeroBits (Seq.fromList xs)
    where
      f :: Bitset n -> Int -> Bool -> Bitset n
      f bs _ False = bs
      f bs i True = setBit bs i
  toList (Bitset xs) = Vec.toList xs

instance (SNatI (FromGHC n)) => Bits (Bitset n) where
  Bitset xs .&. Bitset ys = Bitset (Vec.zipWith (.&.) xs ys)
  Bitset xs .|. Bitset ys = Bitset (Vec.zipWith (.|.) xs ys)
  Bitset xs `xor` Bitset ys = Bitset (Vec.zipWith xor xs ys)
  complement (Bitset xs) = Bitset (fmap complement xs)
  zeroBits = Bitset $ Vec.repeat zeroBits
  bit n = Bitset $ Vec.tabulate (\i -> toNat i == fromIntegral n)
  setBit bs i = bs .|. bit i
  testBit bs i = (bs .&. bit i) /= zeroBits
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned (Bitset xs) = Vec.reverse xs Vec.! 0
  bs@(Bitset xs) `shiftL` n
    | n >= finiteBitSize bs = zeroBits
    | otherwise = Bitset (Vec.tabulate shifted)
    where
      shifted :: Fin (FromGHC n) -> Bool
      shifted i = fromEnum i >= n && i - toEnum n >= 0 && Vec.unVec xs (i - toEnum n)
  unsafeShiftL = shiftL
  bs@(Bitset xs) `shiftR` n
    | n >= finiteBitSize bs = zeroBits
    | otherwise = Bitset (Vec.tabulate shifted)
    where
      shifted :: Fin (FromGHC n) -> Bool
      shifted i = fromEnum i + n < finiteBitSize bs && Vec.unVec xs (i + toEnum n)
  unsafeShiftR = shiftR
  bs@(Bitset xs) `rotateL` n = Bitset (Vec.tabulate rotated)
    where
      -- TODO: too tired to think, check this math. I'm sure I fucked up,
      -- but I also know `mod` is what we want. That much is too obvious.
      rotated :: Fin (FromGHC n) -> Bool
      rotated i = Vec.unVec xs (i - toEnum n `mod` toEnum (finiteBitSize bs))

  -- Bitset xs `rotateR` n = Bitset (Vec.tabulate $ \i -> )
  popCount (Bitset xs) = sum (fmap fromEnum xs)

instance (SNatI (FromGHC n)) => FiniteBits (Bitset n) where
  finiteBitSize _ = reflectToNum (snat @(FromGHC n))
  countLeadingZeros (Bitset xs) = fromMaybe 0 $ Vec.foldr (bool (fmap (+ 1)) id) (Just 0) xs
  countTrailingZeros (Bitset xs) = fromMaybe 0 $ Vec.foldl' (flip $ bool (fmap (+ 1)) id) (Just 0) xs
