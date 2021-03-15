module DS2021Spring.IsomorphicTree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

main :: IO ()
main = do 
  nodes1 <- getNodes
  nodes2 <- getNodes
  let tree1 = constructTree nodes1
  let tree2 = constructTree nodes2
  if compareTree tree1 tree2 then
    putStrLn "Yes"
  else
    putStrLn "No"

compareTree :: Tree String -> Tree String -> Bool
compareTree (Node label1 left1 right1) (Node label2 left2 right2)  = label1 == label2 && ( 
                                                  compareTree left1 left2 && compareTree right1 right2 || 
                                                  compareTree left1 right2 && compareTree left2 right1)
compareTree Empty                       Empty                      = True
compareTree _                           _                          = False


constructTree :: [(Int, (String, Maybe Int, Maybe Int))] -> Tree String
constructTree []      = Empty
constructTree records =  constructNode (Just index) records
      where index = findRootIndex records

constructNode :: Maybe Int -> [(Int, (String, Maybe Int, Maybe Int))] -> Tree String
constructNode Nothing records = Empty
constructNode (Just index) records = Node label (constructNode left records) (constructNode right records)
                                     where (_, (label, left, right)) = records !! index

findRootIndex :: [(Int, (String, Maybe Int, Maybe Int))] -> Int
findRootIndex records = head [x | x <- [0..maxIndex], not . (x `elem`) $ indexes]
          where maxIndex = length (records) - 1
                indexes = collectIndexes records

readLines :: Int -> Int -> IO [(Int, (String, Maybe Int, Maybe Int))]
readLines i n = do
    if i == n then return []
    else do
      line <- getLine
      let (label:left:right:_) = words line
      result <- readLines (i+1) n 
      return ((i,(label,convert left, convert right)):result)

convert :: String -> Maybe Int
convert s = if s == "-" then Nothing
            else Just (read s)

getNodes ::  IO [(Int, (String, Maybe Int, Maybe Int))]
getNodes = do
    line <- getLine
    readLines 0 (read line)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

collectIndexes :: [(Int, (String, Maybe Int, Maybe Int))] -> [Int]
collectIndexes records = foldr (\(_, (_, x , y)) result -> maybeToList x ++ maybeToList y ++ result)   [] records