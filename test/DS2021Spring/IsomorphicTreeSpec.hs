module DS2021Spring.IsomorphicTreeSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.IsomorphicTree


tree1 :: Tree String 
tree1 = Node "A" (Node "B" (Node "D" Empty Empty) 
                            (Node "E" (Node "F" Empty Empty) 
                                      Empty)) 
                 (Node "C" (Node "G" (Node "H" Empty Empty) 
                                     Empty) 
                            Empty)

tree2 :: Tree String 
tree2 = Node "A" (Node "C" (Node "G" Empty 
                                     (Node "H" Empty Empty)) 
                           Empty) 
                 (Node "B" (Node "E" (Node "F" Empty Empty) 
                                     Empty) 
                           (Node "D" Empty Empty))

tree3 :: Tree String 
tree3 =  Node "A" (Node "B" (Node "D" Empty Empty) 
                            (Node "E" (Node "F" Empty Empty) 
                                      Empty)) 
                  (Node "C" (Node "G" (Node "H" Empty Empty) 
                                      Empty) 
                             Empty)

tree4 :: Tree String 
tree4 = Node "A" (Node "B" (Node "G" Empty 
                                     (Node "H" Empty Empty)) 
                           Empty) 
                 (Node "C" (Node "D" (Node "F" Empty Empty) 
                                     Empty) 
                           (Node "E" Empty Empty))


spec :: Spec
spec = do

  describe "isomorphic tree" $ do
    it "True" $
      compareTree tree1 tree2 `shouldBe` True

    it "False" $
      compareTree tree3 tree4 `shouldBe` False