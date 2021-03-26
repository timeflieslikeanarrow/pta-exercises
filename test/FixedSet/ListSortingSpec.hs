module FixedSet.ListSortingSpec where

import Test.Hspec
import Test.QuickCheck

import FixedSet.ListSorting

spec :: Spec
spec = do
  describe "buildRecord" $ do
    it "000002 James 90" $
      buildRecord "000002 James 90" `shouldBe` StudentRecord "000002" "James" 90
  
  describe "sortCriteria 2" $ do
    it "000007 James 85 vs 000010 Amy 90" $
      sortCriteria 2 (buildRecord "000007 James 85") (buildRecord "000010 Amy 90") `shouldBe` GT

  describe "sortCriteria 3" $ do
    it "000007 James 85 vs 000010 Amy 90" $
      sortCriteria 3 (buildRecord "000007 James 85") (buildRecord "000010 Amy 90") `shouldBe` LT

    it "000010 James 85 vs 000001 Amy 85" $
      sortCriteria 3 (buildRecord "000010 James 85") (buildRecord "000001 Amy 85") `shouldBe` GT
