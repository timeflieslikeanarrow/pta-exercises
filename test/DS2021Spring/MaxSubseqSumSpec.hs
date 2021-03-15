module DS2021Spring.MaxSubseqSumSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.MaxSubseqSum

spec :: Spec
spec = do
  describe "maxSubSeqSum" $ do
    it "-2 11 -4 13 -5 -2" $
      maxSubSeqSum [-2, 11, -4, 13, -5, -2] 0 0 `shouldBe` 20