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
                          | otherwise = Node y l                (insertTree r x)

compareTree :: Tree Int -> Tree Int -> Bool
compareTree t1 t2 = t1 == t2

--compare two lists directly
main2 :: IO ()
main2 = interact (intercalate "\n". map (\x -> if x then "Yes" else "No") . process2 . lines)

process2 :: [String] -> [Bool]
process2 (first:rest) = if n == 0 
                        then [] 
                        else 
                             processCase2 (take (k+1) rest) ++ process2 (drop (k+1) rest)
                        where (x:xs) = words first
                              n = read x 
                              k = read $ head xs
                      
processCase2 :: [String] -> [Bool]
processCase2 (first:rest) = map (compareList initial . convertLine) rest
                            where initial = convertLine first

compareList :: [Int] -> [Int] -> Bool
compareList []     []             = True
compareList (x:xs) (y:ys)| x == y = compareList ls ls2 && compareList rs rs2
                                    where ls = filter (< x) xs
                                          rs = filter (> x) xs
                                          ls2 = filter (< y) ys
                                          rs2 = filter (> y) ys
compareList _       _             = False


--use an initial tree with flags to indicate whether the nodes are visited before
main3 :: IO ()
main3 = interact (intercalate "\n". map (\x -> if x then "Yes" else "No") . process3 . lines)

data Tree2 a = Empty2 | Node2 a Bool (Tree2 a) (Tree2 a) deriving (Eq, Show)

process3 :: [String] -> [Bool]
process3 (first:rest) = if n == 0 
                        then [] 
                        else 
                             processCase3 (take (k+1) rest) ++ process3 (drop (k+1) rest)
                        where (x:xs) = words first
                              n = read x 
                              k = read $ head xs
                      
processCase3 :: [String] -> [Bool]
processCase3 (first:rest) = map (compareTreeWithList initial . convertLine) rest
                            where initial = buildTree2 (convertLine first)

buildTree2 :: [Int] -> Tree2 Int
buildTree2 = foldl insertTree2 Empty2 

insertTree2 :: Tree2 Int -> Int -> Tree2 Int
insertTree2 Empty2          x              = Node2 x False Empty2 Empty2
insertTree2 (Node2 y b l r) x | x == y     = Node2 y b l r
                              | x < y      = Node2 y b (insertTree2 l x) r
                              | otherwise  = Node2 y b l                 (insertTree2 r x)


compareTreeWithList :: Tree2 Int -> [Int] -> Bool
compareTreeWithList _ []     = True
compareTreeWithList t (x:xs) = let (result, t2) = compareTreeWithElement t x 
                               in if result then compareTreeWithList t2 xs
                                  else False 
                              

compareTreeWithElement :: Tree2 Int -> Int -> (Bool, Tree2 Int)
compareTreeWithElement t@(Node2 y b l r) x | b && y == x = (False, t)  --duplicate element
                                           | b           = if x < y then 
                                                               let (result, newl) = compareTreeWithElement l x
                                                               in (result, Node2 y b newl r) 
                                                           else 
                                                               let (result, newr) = compareTreeWithElement r x
                                                               in (result, Node2 y b l newr)
                                           | y == x      = (True, Node2 y True l r)
                                           | otherwise   = (False, t) 
compareTreeWithElement t            x               = (False, t)