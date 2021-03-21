module DS2021Spring.CompleteBSTSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.CompleteBST

tree :: Tree Int
tree = Node 6 (Node 3 (Node 1 (Node 0 Empty Empty)
                              (Node 2 Empty Empty))
                      (Node 5 (Node 4 Empty Empty) 
                              Empty))
              (Node 8 (Node 7 Empty Empty)
                      (Node 9 Empty Empty))

tree2 :: Tree Int
tree2 = Node 7 (Node 3 (Node 1 (Node 0 Empty Empty)
                               (Node 2 Empty Empty))
                       (Node 5 (Node 4 Empty Empty) 
                               (Node 6 Empty Empty)))
               (Node 9 (Node 8 Empty Empty)
                       (Node 10 Empty Empty))

tree3 :: Tree Int
tree3 = Node 7 (Node 3 (Node 1 (Node 0 Empty Empty)
                               (Node 2 Empty Empty))
                       (Node 5 (Node 4 Empty Empty) 
                               (Node 6 Empty Empty)))
               (Node 10 (Node 9 (Node 8 Empty Empty) 
                                Empty)
                        (Node 11 Empty Empty))

tree4 :: Tree Int
tree4 = Node 7 (Node 3 (Node 1 (Node 0 Empty Empty)
                               (Node 2 Empty Empty))
                      (Node 5 (Node 4 Empty Empty) 
                              (Node 6 Empty Empty)))
              (Node 11 (Node 9 (Node 8 Empty Empty) 
                               (Node 10 Empty Empty))
                      (Node 12 Empty Empty))

tree5 :: Tree Int
tree5 = Node 7 (Node 3 (Node 1 (Node 0 Empty Empty)
                               (Node 2 Empty Empty))
                       (Node 5 (Node 4 Empty Empty) 
                               (Node 6 Empty Empty)))
              (Node 11 (Node 9 (Node 8 Empty Empty) 
                               (Node 10 Empty Empty))
                       (Node 13 (Node 12 Empty Empty) 
                                Empty))

tree6 :: Tree Int
tree6 = Node 7 (Node 3 (Node 1 (Node 0 Empty Empty)
                               (Node 2 Empty Empty))
                      (Node 5 (Node 4 Empty Empty) 
                              (Node 6 Empty Empty)))
              (Node 11 (Node 9 (Node 8 Empty Empty) 
                               (Node 10 Empty Empty))
                       (Node 13 (Node 12 Empty Empty) 
                               (Node 14 Empty Empty)))

spec :: Spec
spec = do
  describe "buildTree" $ do
    it "complete bs for 10 nodes" $
      buildTree 10 [0..9] `shouldBe` tree

    it "complete bs for 11 nodes" $
      buildTree 11 [0..10] `shouldBe` tree2

    it "complete bs for 12 nodes" $
      buildTree 12 [0..11] `shouldBe` tree3

    it "complete bs for 13 nodes" $
      buildTree 13 [0..12] `shouldBe` tree4

    it "complete bs for 14 nodes" $
      buildTree 14 [0..13] `shouldBe` tree5

    it "complete bs for 15 nodes" $
      buildTree 15 [0..14] `shouldBe` tree6

  describe "traverseTree" $ do
    it "10 nodes" $
      traverseTree ([tree],[]) `shouldMatchList` [6, 3, 8, 1, 5, 7, 9, 0, 2, 4]

    it "11 nodes" $
      traverseTree ([tree2],[]) `shouldMatchList` [7, 3, 9, 1, 5, 8, 10, 0, 2, 4, 6]

    it "12 nodes" $
      traverseTree ([tree3],[]) `shouldMatchList` [7, 3, 10, 1, 5, 9, 11, 0, 2, 4, 6, 8]

    it "13 nodes" $
      traverseTree ([tree4],[]) `shouldMatchList` [7, 3, 11, 1, 5, 9, 12, 0, 2, 4, 6, 8, 10]

    it "14 nodes" $
      traverseTree ([tree5],[]) `shouldMatchList` [7, 3, 11, 1, 5, 9, 13, 0, 2, 4, 6, 8, 10, 12]

    it "15 nodes" $
      traverseTree ([tree6],[]) `shouldMatchList` [7, 3, 11, 1, 5, 9, 13, 0, 2, 4, 6, 8, 10, 12, 14]