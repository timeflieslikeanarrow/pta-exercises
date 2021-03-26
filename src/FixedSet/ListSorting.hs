module FixedSet.ListSorting where

import Data.List 

data StudentRecord = StudentRecord String String Int deriving (Eq, Ord)

instance Show StudentRecord where
  show (StudentRecord studentID name grade) = studentID ++ " " ++ name ++ " " ++ show grade

main :: IO ()
main = interact process

process :: String -> String
process input = intercalate "\n" $ map show $ sortByFieldType (read c) stuRecords
                where (params:records) = lines input
                      (_:c:_) = words params
                      stuRecords = map buildRecord records

buildRecord :: String -> StudentRecord 
buildRecord record = let (studentID:name:grade:_) = words record
                     in StudentRecord studentID name (read grade)

sortByFieldType :: Int -> [StudentRecord] -> [StudentRecord]
sortByFieldType c = sortBy (\s1 s2 -> sortCriteria c s1 s2)                                                                                                                            

sortCriteria :: Int -> StudentRecord -> StudentRecord -> Ordering
sortCriteria 1 (StudentRecord sid1 _ _) (StudentRecord sid2 _ _)           =  sid1 `compare` sid2
sortCriteria 2 (StudentRecord sid1 name1 _) (StudentRecord sid2 name2 _)   = case name1 `compare` name2 of
                                                                                    LT -> LT
                                                                                    EQ -> sid1 `compare` sid2
                                                                                    GT -> GT
sortCriteria 3 (StudentRecord sid1 _ grade1) (StudentRecord sid2 _ grade2) = case grade1 `compare` grade2 of
                                                                                    LT -> LT
                                                                                    EQ -> sid1 `compare` sid2
                                                                                    GT -> GT

sortCriteria _ s1 s2                                                       = s1 `compare` s2
