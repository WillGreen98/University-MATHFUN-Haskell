import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

-- Tuples
sumDifference :: Int -> Int -> (Int,Int)

grade :: StudentMark -> Char

capMark :: StudentMark -> StudentMark

--  Lists and Strings
firstNumbers :: Int -> [Int]

firstSquares :: Int -> [Int]

capitalise :: String -> String

onlyDigits :: String -> String

capMarks :: [StudentMark] -> [StudentMark]
capMarks [("Jo",37), ("Sam",76)] = [("Jo", 37), ("Sam", 40)]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents [("Jo",47), ("Sam",76)] = [("Jo",’D’), ("Sam",’A’)]

-- Using Recursion
duplicate:: String -> Int -> String

divisors :: Int -> [Int]

isPrime :: Int -> Bool

split:: [(a,b)] -> ([a],[b])
split [(1,’a’), (2,’b’), (3,’c’)] = ([1,2,3], "abc")