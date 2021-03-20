module FixedSet.BuildingBST where

import Data.List 

data Tree a = Empty | Node a (Tree a ) (Tree a) deriving (Eq, Show, Ord)

main :: IO ()
main = interact processLines

processLines :: String -> String
processLines input =  intercalate " " $ map show labels
                      where inputList = lines input
                            (n:ns) = inputList
                            nodeCount = read n
                            nodes = map (map read . words) $ take nodeCount ns
                            treeWith0Label = buildTree nodes 0
                            keys = sort $ map read $ words $ head $ drop nodeCount ns
                            finalTree = assignLabel keys treeWith0Label
                            labels = traverseTree finalTree

buildTree :: [[Int]] -> Int -> Tree Int
buildTree ns n | n < 0     = Empty
               | otherwise = Node 0 (buildTree ns l) (buildTree ns r)
                             where  (l:r:_) = ns !! n

assignLabel :: [a] -> Tree a -> Tree a
assignLabel _ Empty         = Empty 
assignLabel xs (Node x l r) = Node label (assignLabel (take leftNodesCount xs) l) (assignLabel (drop (leftNodesCount + 1) xs) r)
                              where leftNodesCount = countNodes l
                                    label = xs !! leftNodesCount

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node a l r) = 1 + countNodes l + countNodes r
                  
type Queue a = ([a], [a])

traverseTree :: Tree a -> [a]
traverseTree t = reverse $ levelTraverse ([t], []) []

levelTraverse :: Queue (Tree a) -> [a] -> [a]
levelTraverse ([], [])              labels = labels
levelTraverse ([], ys)              labels = levelTraverse (reverse ys, []) labels
levelTraverse (Empty:xs, ys)        labels = levelTraverse (xs, ys) labels
levelTraverse ((Node x l r):xs, ys) labels = levelTraverse (xs, r:l:ys) (x:labels)
