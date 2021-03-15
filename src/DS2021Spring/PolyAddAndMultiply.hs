module DS2021Spring.PolyAddAndMultiply where

import Data.List  --(intercalate, sort, groupBy)

swapPair :: [(Int, Int)] -> [(Int, Int)]
swapPair = map (\(x,y) -> (y,x))

groupEveryTwo :: [Int] -> [(Int, Int)]
groupEveryTwo [] = []
groupEveryTwo [_] = []
groupEveryTwo (x:y:xs) = (x,y) : groupEveryTwo xs

sumAB :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
sumAB a b = reverse $ swapPair $ filter (\(x,y) -> y /= 0) $ map (foldr (\(x,y) (rx,ry) -> (x, y + ry)) (0,0)) $ groupBy (\(x1,y1) (x2,y2) -> x1==x2) $ sort $ a' ++ b'
      where a' = swapPair a 
            b' = swapPair b 

allPairs ::  [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
allPairs xs ys = [(x1+x2, y1 * y2) |(x1, y1) <- xs, (x2, y2) <- ys]

multiplyAB ::  [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
multiplyAB a b = reverse $ swapPair $ filter (\(x,y) -> y /= 0) $ map (foldr (\(x,y) (rx,ry) -> (x, y + ry)) (0,0)) $ groupBy (\(x1,y1) (x2,y2) -> x1==x2) $ sort $ allPairs a' b'
     where a' = swapPair a 
           b' = swapPair b 

toString :: [(Int, Int)] -> String
toString xs = intercalate " " (map (\(x, y) -> show x ++ " " ++ show y) xs)

getNumbers :: IO [Int]
getNumbers = do
   line <- getLine
   return  ((map read . words) line)

main :: IO ()
main = do
  numbers1 <- getNumbers
  let a = groupEveryTwo (drop 1 numbers1)
  numbers2 <- getNumbers
  let b = groupEveryTwo (drop 1 numbers2)
  let multiplyResult = multiplyAB a b
  if length multiplyResult == 0 then
    putStrLn "0 0"
  else
    putStrLn (toString multiplyResult)
  let sumResult = sumAB a b
  if length sumResult == 0 then
    putStrLn "0 0"
  else
    putStrLn (toString sumResult)