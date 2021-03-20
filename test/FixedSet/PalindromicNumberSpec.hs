module FixedSet.PalindromicNumberSpec where

import Test.Hspec
import Test.QuickCheck

import FixedSet.PalindromicNumber

spec :: Spec
spec = do
  describe "palindromicNumber" $ do
    it "67 3" $
      palindromicNumber 67 3 `shouldBe` (484, 2)
    
    it "12345" $
      palindromicNumber 69 3 `shouldBe` (1353, 3)