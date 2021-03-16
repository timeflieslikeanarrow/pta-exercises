module DS2021Spring.PopSequence (main, isPopSequence) where

main :: IO ()
main = do  
  line1 <- getLine
  let (m:n:k:_) = words line1
  --putStrLn (show m ++ " " ++ show n ++ " " ++ show k)
  handleLine (read m) (read n) (read k)

isPopSequence :: Int -> Int -> [Int] -> Bool
isPopSequence m n numbers = matchingSequence m n 1 ([], numbers)

matchingSequence :: Int -> Int -> Int -> ([Int], [Int]) -> Bool
matchingSequence m n x (xs, ys) | x > n             = null xs && null ys
                                | length (x:xs) > m = False
                                | otherwise         = matchingSequence m n (x + 1) (matching (x:xs, ys)) 

handleLine :: Int -> Int -> Int -> IO ()
handleLine m n k = do
      if k == 0 then 
        return ()
      else do
        numbers <- readNumbers
        let result = isPopSequence m n numbers
        if result then
          putStrLn "YES"
        else
          putStrLn "NO"
        handleLine m n (k-1)


readNumbers :: IO [Int]
readNumbers = do
      line <- getLine
      return ((map read . words) line)
       
matching :: ([Int], [Int]) -> ([Int], [Int])
matching ([], xs) = ([], xs)
matching (xs, []) = (xs, [])
matching (x:xs, y:ys) = if x == y then matching (xs, ys) else (x:xs, y:ys)


  
