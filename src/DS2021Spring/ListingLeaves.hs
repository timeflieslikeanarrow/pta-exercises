module DS2021Spring.ListingLeaves where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

type Queue a = ([a], [a])

main :: IO ()
main = do
  line <- getLine
  nodes <- readLines 0 (read line)
  let tree = buildTree nodes
  let values = visit ([tree], [])
  display "" values

readLines:: Int -> Int -> IO [(Int, Maybe Int, Maybe Int)]
readLines i n = do
          if i == n then return []
          else
            do line <- getLine
               let (left:right:_) = words line
               result <- readLines (i+1) n
               return ((i,convert left, convert right) : result)

convert :: String -> Maybe Int
convert "-" = Nothing
convert xs  = Just (read xs)

buildTree :: [(Int, Maybe Int, Maybe Int)] -> Tree Int
buildTree nodes = buildNode (Just rootIndex) nodes
                  where rootIndex = findRootIndex nodes
                        root = nodes !! rootIndex


buildNode ::  Maybe Int -> [(Int, Maybe Int, Maybe Int)] -> Tree Int
buildNode Nothing  _     = Empty
buildNode (Just n) nodes = Node index (buildNode left nodes) (buildNode right nodes)
                           where (index, left, right) = nodes !! n
                                         
mapIndex :: Maybe Int -> [Int]
mapIndex Nothing  = []
mapIndex (Just n) = [n]

findRootIndex ::  [(Int, Maybe Int, Maybe Int)] -> Int
findRootIndex nodes  = head [ i | i <- [0..n], not $ i `elem` indexes]
                       where indexes = concat $ map (\(_, left, right) -> mapIndex left ++ mapIndex right) nodes
                             n = length nodes - 1


visit :: Queue (Tree Int) -> [Int]
visit ([], [])                            = []
visit ([], nodes)                            = visit (reverse nodes, [])
visit (Empty:nodes, nodes2)               = visit (nodes, nodes2)
visit ((Node i left right):nodes, nodes2) = case (left, right) of
                                                 (Empty, Empty) -> i : visit(nodes, nodes2)
                                                 _              -> visit(nodes, right:left:nodes2) 


display :: String -> [Int] -> IO ()
display _ []          = do putStrLn ""
display indent (x:xs) = do 
                           putStr (indent ++ show x)
                           display " " xs

