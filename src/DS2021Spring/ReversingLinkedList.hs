module DS2021Spring.ReversingLinkedList where

main :: IO ()
main = do
  line <- getLine
  let (startingAddress:nodeCount:reverseSize:_) = words line
  nodes <- readLines (read nodeCount)
  --putStrLn (show (length nodes))
  let list = buildList startingAddress nodes
  let results = reverseLinkedList (read reverseSize) list
  displayNodes results

reverseLinkedList :: Int -> [(String, Int, String)] -> [(String, Int, String)]
reverseLinkedList k list = updateNodes $ reverseIterator k list

reverseIterator :: Int -> [(String, Int, String)] -> [(String, Int, String)]
reverseIterator k list 
   | length list < k = list
   | otherwise       = (reverse $ take k list) ++ (reverseIterator k (drop k list))

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

buildList :: String -> [(String, Int, String)] -> [(String, Int, String)]
buildList "-1"    _   = []
buildList address addresses  = node : buildList next addresses
                        where node@(_,_, next) = findNode address addresses

displayNodes :: [(String, Int, String)] -> IO ()
displayNodes []           = return ()
displayNodes [(a,v,n)]    = do putStrLn (a ++ " " ++ show v ++ " -1")
displayNodes ((a,v,n):(a2,v2,n2):xs) = do putStrLn (a ++ " " ++ show v ++ " " ++ a2)
                                          displayNodes ((a2,v2,n2):xs)

updateNodes :: [(String, Int, String)] -> [(String, Int, String)]
updateNodes [] = []
updateNodes [(address, value, next)] = [(address, value, "-1")]
updateNodes ((address1, value1, next1):(address2, value2, next2):addresses) = 
  (address1, value1, address2): updateNodes ((address2, value2, next2):addresses) 