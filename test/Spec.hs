import qualified BitsetSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bitset" BitsetSpec.spec
