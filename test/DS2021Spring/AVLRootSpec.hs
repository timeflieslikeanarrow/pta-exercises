module DS2021Spring.AVLRootSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.AVLRoot

spec :: Spec
spec = do
  describe "rotate" $ do
    it "rotateRight" $ do
      let tree = Node 88 3 (Node 70 2 (Node 61 1 Empty Empty) Empty) Empty 
      rotateRight tree `shouldBe`  Node 70 2 (Node 61 1 Empty Empty) (Node 88 1 Empty Empty)

    it "rotateRight" $ do
      let tree = Node 96 3 (Node 88 2 Empty (Node 90 1 Empty Empty)) (Node 120 1 Empty Empty) 
      rotateRight tree `shouldBe`  Node 88 3 Empty (Node 96 2 (Node 90 1 Empty Empty) (Node 120 1 Empty Empty)) 

    it "rotateLeft" $ do
      let tree = Node 88 3 Empty (Node 96 2 Empty (Node 120 1 Empty Empty))
      rotateLeft tree `shouldBe` Node 96 2 (Node 88 1 Empty Empty) (Node 120 1 Empty Empty)

  describe "buildTree" $ do
    it "[88, 70, 61]" $
      buildTree [88, 70, 61] `shouldBe`  Node 70 2 (Node 61 1 Empty Empty) (Node 88 1 Empty Empty) 

    it "[88, 70, 61, 96, 120]" $
      buildTree [88, 70, 61, 96, 120] `shouldBe`  Node 70 3 (Node 61 1 Empty Empty) (Node 96 2 (Node 88 1 Empty Empty) (Node 120 1 Empty Empty))

    it "[88, 70, 61, 96, 120, 90]" $
      buildTree [88, 70, 61, 96, 120, 90] `shouldBe`  Node 88 3 (Node 70 2 (Node 61 1 Empty Empty) Empty) (Node 96 2 (Node 90 1 Empty Empty) (Node 120 1 Empty Empty))

    it "[88, 70, 61, 96, 120, 90, 65]" $
      buildTree [88, 70, 61, 96, 120, 90, 65] `shouldBe`  Node 88 3 (Node 65 2 (Node 61 1 Empty Empty) (Node 70 1 Empty Empty)) (Node 96 2 (Node 90 1 Empty Empty) (Node 120 1 Empty Empty))
