module FixedSet.SpellItRight where

import Data.Char
import Data.List

main :: IO ()
main = interact digitSumInWords

digitSumInWords :: String -> String
digitSumInWords input = intercalate " " $ map (numbers !!) sumDigits
              where digits = map digitToInt $ filter (not . isSpace) input
                    digitSum = sum digits
                    sumDigits = map digitToInt (show digitSum)

numbers :: [String]
numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
