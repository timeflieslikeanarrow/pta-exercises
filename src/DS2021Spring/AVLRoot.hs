module DS2021Spring.AVLRoot where

import Data.List

data Tree a = Empty | Node a Int (Tree a) (Tree a) deriving (Show, Eq)

main :: IO ()
main = interact process

process :: String -> String
process input = show label
        where (_:second:_) = lines input
              nodes = map read $ words second
              tree@(Node label _ _ _ ) = buildTree nodes

height :: Tree Int -> Int
height Empty        = 0
height (Node _ h _ _) = h

updateHeight :: Tree Int -> Tree Int
updateHeight Empty = Empty
updateHeight (Node label _ l r) = Node label (max (height l) (height r) + 1) l r

balanceFactor :: Tree Int -> Int
balanceFactor Empty = 0
balanceFactor (Node label _ l r) = height l - height r

buildTree :: [Int] ->  Tree Int
buildTree = foldl (\tree element -> insertNode element tree) Empty 

rotateLeft :: Tree Int -> Tree Int
rotateLeft (Node label1 h1 l1 
                           (Node label2 h2 l2 
                                           r2))
             = let leftChild = Node label1 h1 l1 l2
               in updateHeight $ Node label2 h2 (updateHeight leftChild) r2
rotateLeft t = t 

rotateRight :: Tree Int -> Tree Int
rotateRight (Node label1 h1 (Node label2 h2 l2
                                            r2)
                            r1)
                    = let rightChild = Node label1 h1 r2 r1
                      in updateHeight $ Node label2 h2 l2 (updateHeight rightChild)
rotateRight t       = t

compareWithLeftChild :: Int -> Tree Int -> Ordering
compareWithLeftChild n (Node _ _ (Node label _ _ _ ) _) = n `compare` label
compareWithLeftChild _ _                                 = EQ

compareWithRightChild :: Int -> Tree Int -> Ordering
compareWithRightChild n (Node _ _ _ (Node label _ _ _ )) = n `compare` label
compareWithRightChild _ _                                 = EQ

singleton :: Int -> Tree Int
singleton label = Node label 1 Empty Empty

insertNode :: Int -> Tree Int -> Tree Int
insertNode n Empty                = singleton n
insertNode n t@(Node label h l r) = if n == label 
                                    then t
                                    else let node = if n < label
                                                    then Node label h (insertNode n l) r 
                                                    else Node label h l                (insertNode n r)
                                            in let node2@(Node newlabel newh newl newr) = updateHeight node
                                            in let balance = balanceFactor node2
                                            in if balance > 1 then
                                                    case n `compareWithLeftChild` node2 of
                                                      EQ -> node2
                                                      LT -> rotateRight node2
                                                      GT -> let leftChild = rotateLeft newl in rotateRight (Node newlabel newh leftChild newr)
                                                else if balance < (-1) then
                                                    case n `compareWithRightChild` node2 of
                                                      EQ -> node2
                                                      LT -> let rightChild = rotateRight newr in rotateLeft (Node newlabel newh newl rightChild)
                                                      GT -> rotateLeft node2
                                                else node2