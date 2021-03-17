module DS2021Spring.TreeTraversal where

import Data.List hiding (findIndex)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

main :: IO ()
main = interact (processLines . tail . lines)
       
processLines :: [String] -> String
processLines lines = intercalate " " $ map show postOrders
                     where (preOrders, inOrders) = extractOrders lines [] ([], [])
                           tree = buildTree (reverse preOrders) (reverse inOrders)
                           postOrders = postTraverse tree
           
extractOrders :: [String] -> [Int] -> ([Int], [Int]) -> ([Int], [Int])
extractOrders []           _     orders      = orders
extractOrders (line:lines) stack (pres, ins) = case op of
                                                "Push" -> extractOrders lines (x:stack) (x:pres, ins)
                                                "Pop"  -> extractOrders lines ys        (pres, y:ins)
                                             where (op:xs) = words line
                                                   x = read (head xs)
                                                   (y:ys) = stack
                                                  
buildTree :: [Int] -> [Int] -> Tree Int
buildTree []     []     = Empty
buildTree (x:xs) ys     = Node x (buildTree (take index xs) (take index ys)) (buildTree (drop index xs) (drop (index + 1) ys)) 
                          where index = findIndex x ys
                                
findIndex :: Int -> [Int] -> Int
findIndex x xs = head [ i | (i, y) <- zip [0..] xs, y == x]

postTraverse :: Tree Int -> [Int]
postTraverse Empty = []
postTraverse (Node n left right) = postTraverse left ++ postTraverse right ++ [n]