module DS2021Spring.SameTreeSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.SameTree

tree :: Tree Int
tree = Node 3 (Node 1 Empty
                      (Node 2 Empty Empty))
              (Node 4 Empty Empty)

spec :: Spec
spec = do
  describe "build tree" $ do
    it "[3,1,4,2]" $
      buildTree [3, 1, 4, 2] `shouldBe` tree

    it "compare trees" $ do
      compareTree (buildTree [3, 1, 4, 2]) (buildTree [3, 4, 1, 2]) `shouldBe` True 
      compareTree (buildTree [3, 1, 4, 2]) (buildTree [3, 2, 4, 1]) `shouldBe` False      

  describe "compare lists directly without building trees" $ do
    it "compare lists" $ do
      compareList [3, 1, 4, 2] [3, 4, 1, 2] `shouldBe` True 
      compareList [3, 1, 4, 2] [3, 2, 4, 1] `shouldBe` False 

  describe "compare tree and list" $ do
    it "compare tree and list" $ do
      compareTreeWithList (buildTree2 [3, 1, 4, 2]) [3, 4, 1, 2] `shouldBe` True 
      compareTreeWithList (buildTree2 [3, 1, 4, 2]) [3, 2, 4, 1] `shouldBe` False 