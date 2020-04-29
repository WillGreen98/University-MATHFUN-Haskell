import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

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

sumDifference :: Int -> Int -> (Int,Int)
sumDifference n1 n2 = (n1 + n2, n1 - n2)

grade :: StudentMark -> Char
grade (std, mrk)
  | mrk < 40 = 'F'
  | mrk <= 49 = 'D'
  | mrk <= 59 = 'C'
  | mrk <= 69 = 'B'
  | mrk >= 70 = 'A'

capMark :: StudentMark -> StudentMark
capMark (std, mrk) = if mrk > 40 then (std, 40) else (std, mrk)

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [n*n | n <- firstNumbers n]

capitalise :: String -> String
capitalise string = [toUpper char | char <- string]

onlyDigits :: String -> String
onlyDigits string = [char | char <- string, isDigit char]

capMarks :: [StudentMark] -> [StudentMark]
capMarks studentList = [capMark (std, mrk) | (std, mrk) <- studentList]
-- OR THIS WAY
-- capMarks studentList = [capMark student | student <- studentList]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents studentList = [(std, grade (std, mrk)) | (std, mrk) <- studentList]

duplicate :: String -> Int -> String
duplicate word times = if times > 0 then word ++ duplicate word (times - 1) else ""

divisors :: Int -> [Int]
divisors n = [number | number <- firstNumbers n, n `mod` number == 0]

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

split :: [(a, b)] -> ([a], [b])
split listOfPairs = (([a | (a, _) <- listOfPairs]), ([b | (_,b) <- listOfPairs]))
