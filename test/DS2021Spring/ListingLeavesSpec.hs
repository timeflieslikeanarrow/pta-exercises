module DS2021Spring.ListingLeavesSpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.ListingLeaves

nodes :: [(Int, Maybe Int, Maybe Int)]
nodes = [(0, Just 1, Nothing),
         (1, Nothing, Nothing),
         (2, Just 0, Nothing),
         (3, Just 2, Just 7),
         (4, Nothing, Nothing),
         (5, Nothing, Nothing),
         (6, Just 5, Nothing),
         (7, Just 4, Just 6)]

spec :: Spec
spec = do
  describe "listing leaves" $ do
    let tree = buildTree nodes
    
    it "every 4" $
      visit ([tree], []) `shouldMatchList` [4, 1, 5]