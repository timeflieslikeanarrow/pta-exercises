module DS2021Spring.CompleteBST where

import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

main :: IO ()
main = interact process

process :: String -> String
process input = intercalate " " $ map show $ traverseTree ([tree],[])
        where (first:second:_) = lines input
              n = read first
              nodes = map read $ words second
              tree = buildTree n (sort nodes)


getFullCount :: Int -> Int
getFullCount n = until (> n) (*2) 1

buildTree :: Int -> [Int] -> Tree Int
buildTree n [] = Empty
buildTree n xs = Node (xs !! leftCount) (buildTree leftCount (take leftCount xs)) (buildTree rightCount (drop (leftCount + 1) xs))
                  where fullCount = getFullCount n
                        previousFullCount = fullCount `div` 2 - 1
                        fullBottomCount = fullCount - 1 - previousFullCount
                        leftBottomCount = min (n - previousFullCount) (fullBottomCount `div` 2)
                        leftCount = previousFullCount `div` 2 + leftBottomCount
                        rightCount = n - leftCount - 1

type Queue a = ([a], [a])

traverseTree :: Queue (Tree Int) -> [Int]
traverseTree ([],[])  = []
traverseTree ([], ys) = traverseTree (reverse ys, [])
traverseTree (Empty:xs, ys) = traverseTree (xs, ys)
traverseTree (Node x l r:xs, ys) = x: traverseTree (xs, r:l:ys) 