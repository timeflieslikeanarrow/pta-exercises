module FixedSet.NumberDoublingSpec where

import Test.Hspec
import Test.QuickCheck

import FixedSet.NumberDoubling

spec :: Spec
spec = do
  describe "isDoubledNumberPermutation" $ do
    it "1234567899" $
      isDoubledNumberPermutation 1234567899 `shouldBe` (2469135798, True)
    
    it "12345" $
      isDoubledNumberPermutation 12345 `shouldBe` (24690, False)