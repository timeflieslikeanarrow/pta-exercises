module ReversiblePrimeSpec where

import Test.Hspec
import Test.QuickCheck

import ReversiblePrime

spec :: Spec
spec = do
  describe "isReversiblePrime" $ do
    it "73 10" $
      isReversiblePrime 73 10 `shouldBe` True

    it "23 2" $ 
      isReversiblePrime 23 2 `shouldBe` True

    it "23 10" $
      isReversiblePrime 23 10 `shouldBe` False

    it "1 10" $
      isReversiblePrime 1 10 `shouldBe` False

    