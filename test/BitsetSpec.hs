module BitsetSpec (spec) where

import Data.Bits
import Data.Bitset
import Test.Hspec

spec :: Spec
spec = do
  let bitset2 = bitset @2

  describe "sanity" $ do
    it "all bits are zero" $ all not (toList (bitset2 zeroBits))
    it "length is two" $ length (toList (bitset2 zeroBits)) == 2

  describe "identities" $ do
    it "can be constructed using lists" $ bitset2 [] == zeroBits
    it "complement of zeroBits is oneBits" $ bitset2 (complement zeroBits) == oneBits
    it "is roundtrippable" $
      let roundtrip :: Bitset 2 -> Bitset 2
          roundtrip = fromList . toList
       in roundtrip [False, True] == [False, True]

  describe "conversions" $ do
    it "fromList mempty to integral" $ bitsetToIntegral (bitset2 $ fromList mempty) == (0 :: Int)
    it "list of ones to integral" $ bitsetToIntegral @2 (bitset2 [True, True]) == (3 :: Int)

  describe "bit fiddling" $ do
    it "sets bit 0" $ bitset2 zeroBits `setBit` 0 == bit 0
    it "sets bit 1" $ bitset2 zeroBits `setBit` 1 == bit 1
    it "sets bit 2 is noop" $ bitset2 zeroBits `setBit` 2 == zeroBits
    it "bitwise and" $ bitset2 oneBits .&. bit 1 == bit 1
    it "bitwise ior" $ bitset2 zeroBits .|. bit 1 == bit 1

  describe "bit shifting" $ do
    it "is useless to shift a bitset all zeros" $ bitset2 zeroBits `shiftL` 10 == zeroBits
    it "is useful to shift a bitset with something" $ bitset2 (bit 0) `shiftL` 1 == bit 1
