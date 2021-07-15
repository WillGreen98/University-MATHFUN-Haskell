-- Worksheet 4 selected model solutions

import Data.Char

type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

-- 1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

--2
grade :: StudentMark -> Char
grade (_,mark)
    | mark >= 70    = 'A'
    | mark >= 60    = 'B'
    | mark >= 50    = 'C'
    | mark >= 40    = 'D'
    | otherwise     = 'F'

--4
firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

-- 6
capitalise :: String -> String
capitalise str = [ toUpper c | c <- str ]

-- 9
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents stList = [(st,grade (st,mk)) | (st,mk) <- stList ]
