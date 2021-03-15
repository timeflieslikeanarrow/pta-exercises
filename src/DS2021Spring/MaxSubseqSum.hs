module DS2021Spring.MaxSubseqSum where

maxSubSeqSum :: [Int] -> Int -> Int -> Int
maxSubSeqSum []     _       maxSum  = maxSum
maxSubSeqSum (x:xs) lastSum maxSum  = maxSubSeqSum xs (max thisSum 0) (max thisSum maxSum)
                                      where thisSum = lastSum + x



main :: IO ()
main = do
  line <- getLine
  let count = read line :: Int
  line2 <- getLine
  let numbers = (map read . words) line2
  putStrLn (show (maxSubSeqSum numbers 0 0))
