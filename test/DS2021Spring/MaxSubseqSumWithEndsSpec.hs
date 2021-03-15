module DS2021Spring.MaxSubseqSumWithEndsSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.MaxSubseqSumWithEnds

spec :: Spec
spec = do
  describe "maxSubSeqSumWithEnds" $ do
    it "-10 1 2 3 4 -5 -23 3 7 -21" $ do
      let numbers = [-10, 1, 2, 3, 4, -5, -23, 3, 7, -21] 
      maxSubseqSumWithEnds numbers (0, minBound, minBound) (-1, head numbers, last numbers) `shouldBe` (10, 1, 4)

    it "all negative" $ do
      let numbers = [-10, -1, -2, -3, -4, -5, -23, -3, -7, -21] 
      maxSubseqSumWithEnds numbers (0, minBound, minBound) (-1, head numbers, last numbers) `shouldBe` (-1, -10, -21)
       