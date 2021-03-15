module DS2021Spring.PolyAddAndMultiplySpec where

import Test.Hspec
import Test.QuickCheck

import DS2021Spring.PolyAddAndMultiply

spec :: Spec
spec = do
  let poly1 = [(3, 4), (-5, 2), (6, 1), (-2, 0)]
  let poly2 = [(5, 20), (-7, 4), (3, 1)]

  describe "sumAB" $ do
    it "poly1 + poly2" $
      sumAB poly1 poly2 `shouldMatchList` [(5, 20), (-4, 4), (-5, 2), (9, 1), (-2, 0)]

  describe "multiplyAB" $ do
    it "poly1 * poly2" $
      multiplyAB poly1 poly2 `shouldMatchList` [(15, 24), (-25, 22), (30, 21), (-10, 20),(-21, 8),(35, 6), 
                                                (-33, 5), (14, 4),(-15, 3), (18, 2), (-6, 1)]