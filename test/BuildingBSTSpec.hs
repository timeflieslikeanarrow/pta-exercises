module BuildingBSTSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List 
import BuildingBST

tree0 :: Tree Int
tree0 = Node 0 (Node 0 (Node 0 Empty Empty) 
                       (Node 0 Empty
                                 (Node 0 (Node 0 Empty Empty)
                                         Empty)))
               (Node 0 (Node 0 Empty (Node 0 Empty Empty))
                       Empty)

tree :: Tree Int
tree = Node 58 (Node 25 (Node 11 Empty Empty) 
                        (Node 38 Empty
                                 (Node 45 (Node 42 Empty Empty)
                                          Empty)))
               (Node 82 (Node 67 Empty (Node 73 Empty Empty))
                        Empty)


spec :: Spec
spec = do
  describe "building binary search tree" $ do
    it "countNodes" $
      countNodes tree `shouldBe` 9

    it "buildTree" $
      buildTree [[1,6],[2,3],[-1, -1],[-1, 4],[5, -1],[-1, -1],[7, -1],[-1, 8],[-1, -1]] 0 `shouldBe` tree0

    it "assignLabel" $
      assignLabel (sort [73, 45, 11, 58, 82, 25, 67, 38, 42]) tree0 `shouldBe` tree

    it "traverseTree" $
      traverseTree tree `shouldMatchList` [58, 25, 82, 11, 38, 67, 45, 73, 42]

    it "processLines" $
      processLines "9\n1 6\n2 3\n-1 -1\n-1 4\n5 -1\n-1 -1\n7 -1\n-1 8\n-1 -1\n73 45 11 58 82 25 67 38 42" `shouldBe` "58 25 82 11 38 67 45 73 42"