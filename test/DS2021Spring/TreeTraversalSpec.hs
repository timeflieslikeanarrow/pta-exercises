module DS2021Spring.TreeTraversalSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.TreeTraversal

tree :: Tree Int
tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
                                                              (Node 5 (Node 6 Empty Empty) Empty)

spec :: Spec
spec = do
  describe "build tree" $ do
    it "based on pre orders and in orders sequence" $
      buildTree [1,2,3,4,5,6] [3,2,4,1,6,5] `shouldBe` tree

    it "post orders" $
      postTraverse tree `shouldMatchList` [3, 4, 2, 6, 5, 1]
