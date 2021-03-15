module DS2021Spring.MaxSubseqSumWithEnds where

maxSubseqSumWithEnds :: [Int] -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
maxSubseqSumWithEnds []     _       maxSums             = maxSums
maxSubseqSumWithEnds (x:xs) (lastSum, lastStart, _) maxSums@(maxSum, _, _)
                            | thisSum < 0       = maxSubseqSumWithEnds xs (0, minBound, minBound) maxSums
                            | thisSum > maxSum  = maxSubseqSumWithEnds xs (thisSum, newStart, x) (thisSum, newStart, x)
                            | otherwise         = maxSubseqSumWithEnds xs (thisSum, newStart, x) maxSums
                            where thisSum = lastSum + x
                                  newStart = if lastStart == minBound then x else lastStart

main :: IO ()
main = do
    getLine
    line <- getLine
    let numbers = (map read . words) line
    if all (< 0) numbers then
        putStrLn ("0 " ++ show (head numbers) ++ " " ++ show (last numbers))
    else do
        let (sum, start, end) = maxSubseqSumWithEnds numbers (0, minBound, minBound) (-1, head numbers, last numbers)
        putStrLn (show sum ++ " " ++ show start ++ " " ++ show end)