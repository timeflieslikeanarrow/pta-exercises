module DS2021Spring.ReversingLinkedList where

main :: IO ()
main = do
  line <- getLine
  let (startingAddress:nodeCount:reverseSize:_) = words line
  nodes <- readLines (read nodeCount)
  --putStrLn (show (length results))
  let list = buildList startingAddress nodes
  let results = reverseLinkedList (read reverseSize) list
  displayNodes results

reverseLinkedList :: Int -> [(String, Int)] -> [(String, Int)]
reverseLinkedList k list = reverseIterator k (length list) list

reverseIterator :: Int -> Int-> [(String, Int)] -> [(String, Int)]
reverseIterator k n list 
   | n < k      = list
   | otherwise  = (reverse $ take k list) ++ (reverseIterator k (n-k) (drop k list))

readLines :: Int -> IO [(String, Int, String)]
readLines n = do
      if n == 0 then 
        return []
      else do
        line <- getLine
        let (address:value:next:_) = words line
        results <- readLines (n-1)
        return ((address,read value,next):results)

findNode :: String -> [(String, Int, String)] -> (String, Int, String)
findNode address (node@(x,_,_):xs) = if x == address then node else findNode address xs

buildList :: String -> [(String, Int, String)] -> [(String, Int)]
buildList "-1"    _   = []
buildList address addresses  = (a,v) : buildList next addresses
                        where (a,v, next) = findNode address addresses

displayNodes :: [(String, Int)] -> IO ()
displayNodes []                 = return ()
displayNodes [(a,v)]            = do putStrLn (a ++ " " ++ show v ++ " -1")
displayNodes ((a,v):(a2,v2):xs) = do putStrLn (a ++ " " ++ show v ++ " " ++ a2)
                                     displayNodes ((a2,v2):xs)