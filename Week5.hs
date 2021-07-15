-- Worksheet 5 selected model solutions
 
import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- 1
headPlusOne :: [Int] -> Int
headPlusOne []       =  0
headPlusOne (x:xs)   =  x + 1

-- 3
rotate (y:(x:xs))    = x:y:xs
rotate xs            = xs

-- 4
listLength []        = 0
listLength (x:xs)    = 1 + listLength xs

-- 8
removeAll :: Int -> [Int] -> [Int]
removeAll n []       = []
removeAll n (x:xs) 
    | n == x      = removeAll n xs
    | otherwise   = x : removeAll n xs

-- 9
type StudentMark = (String,Int)

listMarks :: String -> [StudentMark] -> [Int]
listMarks st []    = []
listMarks st ((name,mark):stmks)
    | st == name      = mark : listMarks st stmks
    | otherwise       = listMarks st stmks

