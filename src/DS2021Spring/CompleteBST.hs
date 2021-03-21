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

nextFullCount :: Int -> Int
nextFullCount n = until (> n) (*2) 1

buildTree :: Int -> [Int] -> Tree Int
buildTree n [] = Empty
buildTree n xs = Node label (buildTree leftNodeCount (take leftNodeCount xs)) (buildTree rightNodeCount (drop (leftNodeCount + 1) xs))
                  where fullCount = nextFullCount n
                        previousFullCount = fullCount `div` 2 - 1
                        fullBottomCount = fullCount - 1 - previousFullCount
                        leftBottomCount = min (n - previousFullCount) (fullBottomCount `div` 2)
                        leftNodeCount = previousFullCount `div` 2 + leftBottomCount
                        rightNodeCount = n - leftNodeCount - 1
                        label = xs !! leftNodeCount

type Queue a = ([a], [a])

traverseTree :: Queue (Tree Int) -> [Int]
traverseTree ([],[])             = []
traverseTree ([], ys)            = traverseTree (reverse ys, [])
traverseTree (Empty:xs, ys)      = traverseTree (xs, ys)
traverseTree (Node x l r:xs, ys) = x: traverseTree (xs, r:l:ys) 