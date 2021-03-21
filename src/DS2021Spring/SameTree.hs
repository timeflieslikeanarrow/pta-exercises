module DS2021Spring.SameTree where

import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

main :: IO ()
main = interact (intercalate "\n". map (\x -> if x then "Yes" else "No") . process . lines)

process :: [String] -> [Bool]
process (first:rest) = if n == 0 
                       then [] 
                       else 
                            processCase (take (k+1) rest) ++ process (drop (k+1) rest)
                       where (x:xs) = words first
                             n = read x 
                             k = read $ head xs
                      

processCase :: [String] -> [Bool]
processCase (first:rest) = map (compareTree initial . buildTree . convertLine) rest
                           where initial = buildTree (convertLine first)
                             

convertLine :: String -> [Int]
convertLine =  map read . words

buildTree :: [Int] -> Tree Int
buildTree = foldl insertTree Empty 

insertTree :: Tree Int -> Int -> Tree Int
insertTree Empty        x             = Node x Empty Empty
insertTree (Node y l r) x | x == y    = Node y l r
                          | x < y     = Node y (insertTree l x) r
                          | otherwise = Node y l (insertTree r x)

compareTree :: Tree Int -> Tree Int -> Bool
compareTree t1 t2 = t1 == t2
