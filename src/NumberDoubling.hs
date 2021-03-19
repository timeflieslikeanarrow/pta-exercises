module NumberDoubling where

import Data.List

main :: IO ()
main = interact process

process :: String -> String
process line = (if isPerm then "Yes" else "No") ++ "\n" ++ show n2
            where n = read line
                  (n2, isPerm) = isDoubledNumberPermutation n

isDoubledNumberPermutation  :: Integer -> (Integer, Bool)
isDoubledNumberPermutation n = (n2, sort (show n) == sort (show n2))
                  where n2 = 2 * n