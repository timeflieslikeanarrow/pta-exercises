module FixedSet.ListSorting where

import Data.List 

main :: IO ()
main = interact process

process :: String -> String
process input = intercalate "\n" $ map (\(s,n,g) -> s ++ " " ++ n ++ " " ++ show g) $ sortByFieldType (read c) stuRecords
                where (params:records) = lines input
                      (_:c:_) = words params
                      stuRecords = map buildRecord records

buildRecord :: String -> (String, String, Int)
buildRecord record = let (studentID:name:grade:_) = words record
                     in (studentID, name, read grade)

sortByFieldType :: Int -> [(String, String, Int)] -> [(String, String, Int)]
sortByFieldType c = sortBy (\s1 s2 -> sortCriteria c s1 s2)                                                                                                                            

sortCriteria :: Int -> (String, String, Int) -> (String, String, Int) -> Ordering
sortCriteria 1 (sid1, _, _) (sid2, _, _)           =  sid1 `compare` sid2
sortCriteria 2 (sid1, name1, _) (sid2, name2, _)   = case name1 `compare` name2 of
                                                                LT -> LT
                                                                EQ -> sid1 `compare` sid2
                                                                GT -> GT
sortCriteria 3 (sid1, _, grade1) (sid2, _, grade2) = case grade1 `compare` grade2 of
                                                                LT -> LT
                                                                EQ -> sid1 `compare` sid2
                                                                GT -> GT

sortCriteria _ s1 s2                               = s1 `compare` s2
