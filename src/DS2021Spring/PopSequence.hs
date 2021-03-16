module DS2021Spring.PopSequence (main, isPopSequence) where

main :: IO ()
main = do
  line <- getLine
  let (m:n:cases:_) = words line
  processTestCases (read m) (read n) (read cases)

isPopSequence :: Int -> Int -> [Int] -> Bool
isPopSequence m n numbers = popSequenceIterator m n 1 ([], numbers, 0)

popSequenceIterator :: Int -> Int -> Int -> ([Int], [Int], Int) ->  Bool
popSequenceIterator m n x (xs, ys, count) | x > n          =  count == 0 && null ys
                                          | count == m     = False
                                          | otherwise      = popSequenceIterator m n (x+1) (popOffMatchingPrefixe (x:xs, ys, count+1))
                                                       
popOffMatchingPrefixe :: (Eq a) => ([a], [a], Int) -> ([a], [a], Int)
popOffMatchingPrefixe (x:xs, y:ys, count) | x == y = popOffMatchingPrefixe (xs, ys, count-1)
popOffMatchingPrefixe (xs,   ys, count)            = (xs, ys, count)

processTestCases :: Int -> Int -> Int -> IO ()
processTestCases m n k = do
    if k == 0 then 
      return ()
    else do
      line <- getLine
      let numbers = map read $ words line
      if isPopSequence m n numbers then
        putStrLn "YES"
      else
        putStrLn "NO"
      processTestCases m n (k-1)