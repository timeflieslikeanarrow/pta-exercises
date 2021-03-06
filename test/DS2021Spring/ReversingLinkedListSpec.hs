module DS2021Spring.ReversingLinkedListSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.ReversingLinkedList

list :: [(String, Int)]
list = [("00100", 1), 
        ("12300", 2), 
        ("33218", 3), 
        ("00000", 4),
        ("99999", 5), 
        ("68237", 6)]

result ::  [(String, Int)]
result = [("00000", 4),
          ("33218", 3), 
          ("12300", 2),
          ("00100", 1),  
          ("99999", 5), 
          ("68237", 6)]


spec :: Spec
spec = do
  describe "reverseListBy" $ do
    it "every 4" $
      reverseLinkedList 4 list `shouldMatchList` result