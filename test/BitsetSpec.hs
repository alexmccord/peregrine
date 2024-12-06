module BitsetSpec (spec) where

import Data.Bits
import Data.Bitset
import Test.Hspec

spec :: Spec
spec = do
  let bitset2 = bitset @2
  let roundtrip :: Bitset 2 -> Bitset 2
      roundtrip = fromList . toList

  let bs00 = bitset2 zeroBits
  let bs01 = bitset2 (bit 0)
  let bs10 = bitset2 (bit 1)
  let bs11 = bitset2 oneBits

  describe "sanity" $ do
    it "all bits are zero" $ all not (toList bs00)
    it "length is two" $ length (toList bs00) == 2

  describe "identities" $ do
    it "can be constructed using lists" $ bitset2 [] == bs00
    it "is roundtrippable" $ roundtrip [False, True] == [False, True]
    it "complement of bs00 is bs11" $ complement bs00 == bs11

  describe "conversions" $ do
    it "fromList mempty to integral" $ bitsetToIntegral (bitset2 $ fromList mempty) == (0 :: Int)
    it "list of ones to integral" $ bitsetToIntegral (bitset2 [True, True]) == (3 :: Int)
    it "matches with binary literal order" $ bitsetToIntegral (bitset2 [True, False]) == (0b10 :: Int)

  describe "bit fiddling" $ do
    it "sets bit 0" $ bs00 `setBit` 0 == bs01
    it "sets bit 1" $ bs00 `setBit` 1 == bs01
    it "sets bit 2" $ bs00 `setBit` 2 == bs00
    it "bitwise and" $ bs11 .&. bs01 == bs01
    it "bitwise ior" $ bs00 .|. bs01 == bs01

  describe "bit shifting" $ do
    it "is useless to shift a bitset 0b00" $ bs00 `shiftL` 10 == bs00
    it "is useful to shift left 1 bit a bitset 0b01" $ bs01 `shiftL` 1 == bs10
    it "is useful to shift left 1 bit a bitset 0b10" $ bs10 `shiftL` 1 == bs00
    it "is useful to shift right 1 bit a bitset 0b01" $ bs01 `shiftR` 1 == bs00
    it "is useful to shift right 1 bit a bitset 0b10" $ bs10 `shiftR` 1 == bs01

  describe "bit rotating" $ do
    it "is useless to rotate a bitset 0b00" $ bs00 `rotateL` 10 == bs00
    it "is useless to rotate a bitset 0b11" $ bitset2 bs11 `rotateL` 10 == bs11
    it "is useful to rotate left 1 bit a bitset 0b01" $ bs01 `rotateL` 1 == bs10
    it "is useful to rotate left 1 bit a bitset 0b10" $ bs10 `rotateL` 1 == bs01
    it "is useful to rotate right 1 bit a bitset 0b01" $ bs01 `rotateR` 1 == bs10
    it "is useful to rotate right 1 bit a bitset 0b10" $ bs10 `rotateR` 1 == bs01
