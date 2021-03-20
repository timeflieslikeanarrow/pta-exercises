module FixedSet.PalindromicNumber where

main :: IO ()
main = interact process

process :: String -> String
process line = show n2 ++ "\n" ++ show steps
               where (n:k:_) = words line
                     (n2, steps) = palindromicNumber (read n) (read k)


palindromicNumber :: Integer -> Integer -> (Integer, Integer)
palindromicNumber n k = palindromicIterator n k 0

palindromicIterator :: Integer -> Integer -> Integer -> (Integer, Integer)
palindromicIterator n k i | i == k    = (n, k)
                          | otherwise = if isPalindrome n then (n, i) else palindromicIterator n3 k (i+1) 
                                        where n2 = reverseNumber n
                                              n3 = n + n2

isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == reverse (show n)

reverseNumber :: Integer -> Integer
reverseNumber n = read $ reverse . show $ n