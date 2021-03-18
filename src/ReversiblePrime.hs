module ReversiblePrime where

import Data.List

main :: IO ()
main = interact (processLines . init . lines)

processLines :: [String] -> String
processLines lines = intercalate "\n" $ map ((\x -> if x then "Yes" else "No") . processLine) $ lines

processLine :: String -> Bool
processLine line = isReversiblePrime (read n) (read d)
                   where (n:d:_) = words line

isReversiblePrime :: Integer -> Integer -> Bool
isReversiblePrime n d = isPrime n && (isPrime $ convertToDecimal d 0 $ reverse $ convertToDigits n d [])

convertToDigits :: Integer -> Integer -> [Integer] -> [Integer]
convertToDigits 0 _ acc = acc
convertToDigits n d acc = convertToDigits (n `div` d) d ((n `mod` d) : acc)

convertToDecimal :: Integer -> Integer -> [Integer] -> Integer
convertToDecimal d acc []  = acc
convertToDecimal d acc (x:xs) = convertToDecimal d (x+d*acc) xs

isPrime :: Integer -> Bool
isPrime n = n >= 2 && and [ n `mod` x /= 0 | x <- [2..n-1]]
