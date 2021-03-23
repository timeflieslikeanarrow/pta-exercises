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
compareWithLeftChild _ _                                = EQ

compareWithRightChild :: Int -> Tree Int -> Ordering
compareWithRightChild n (Node _ _ _ (Node label _ _ _ )) = n `compare` label
compareWithRightChild _ _                                = EQ

singleton :: Int -> Tree Int
singleton label = Node label 1 Empty Empty

handleLeftInsertion :: Int -> Tree Int -> Tree Int
handleLeftInsertion n node@(Node label h l r) = if balance > 1 then
                                                    case n `compareWithLeftChild` node of
                                                      EQ -> node
                                                      LT -> rotateRight node
                                                      GT -> let leftChild = rotateLeft l in rotateRight (Node label h leftChild r)
                                                else
                                                    node
                                                where balance = balanceFactor node
handleLeftInsertion _ t = t 


handleRightInsertion :: Int -> Tree Int -> Tree Int
handleRightInsertion n node@(Node label h l r) =  if balance < (-1) then
                                                      case n `compareWithRightChild` node of
                                                        EQ -> node
                                                        LT -> let rightChild = rotateRight r in rotateLeft (Node label h l rightChild)
                                                        GT -> rotateLeft node
                                                  else
                                                      node
                                                  where balance = balanceFactor node

handleRightInsertion _ t = t

insertNode :: Int -> Tree Int -> Tree Int
insertNode n Empty                = singleton n
insertNode n t@(Node label h l r) = if n == label 
                                    then t
                                    else if n < label
                                         then handleLeftInsertion  n $ updateHeight $ Node label h (insertNode n l) r 
                                         else handleRightInsertion n $ updateHeight $ Node label h l                (insertNode n r)