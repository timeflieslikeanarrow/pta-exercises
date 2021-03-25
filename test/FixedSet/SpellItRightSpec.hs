module FixedSet.SpellItRightSpec where

import Test.Hspec
import Test.QuickCheck

import FixedSet.SpellItRight

spec :: Spec
spec = do
  describe "digitSumInWords" $ do
    it "12345" $
      digitSumInWords "12345" `shouldBe` "one five"
    