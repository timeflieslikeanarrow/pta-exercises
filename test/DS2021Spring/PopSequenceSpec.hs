module DS2021Spring.PopSequenceSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.PopSequence

spec :: Spec
spec = do
  describe "pop sequence" $ do
    
    it "[1, 2, 3, 4, 5, 6, 7]" $
      isPopSequence 5 7 [1, 2, 3, 4, 5, 6, 7] `shouldBe` True

    it "[3, 2, 1, 7, 5, 6, 4]" $
      isPopSequence 5 7 [3, 2, 1, 7, 5, 6, 4] `shouldBe` False

    it "[7, 6, 5, 4, 3, 2, 1]" $
      isPopSequence 5 7 [7, 6, 5, 4, 3, 2, 1] `shouldBe` False

    it "[5, 6, 4, 3, 7, 2, 1]" $
      isPopSequence 5 7 [5, 6, 4, 3, 7, 2, 1] `shouldBe` True

    it "[1, 7, 6, 5, 4, 3, 2]" $
      isPopSequence 5 7 [1, 7, 6, 5, 4, 3, 2] `shouldBe` False