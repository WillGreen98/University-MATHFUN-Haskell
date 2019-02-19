-- Week5.hs This file illustrates list patterns and recursion over lists.

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd
fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail
head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

--  List Patterns
headPlusOne :: [Int] -> Int
headPlusOne [5,7,2,4] = 6

duplicateHead :: [a] -> [a]
duplicateHead [5,7] = [5,5,7]

rotate :: [a] -> [a]
rotate [5,7,2,4] = [7,5,2,4]

-- Recursion over Lists
listLength :: [a] -> Int

multAll :: [Int] -> Int
multAll [5,2,4] = 40

andAll :: [Bool] -> Bool
andAll [True, True] = True
andAll [True, False] = False

countElems :: Int -> [Int] -> Int
countElems 3 [5, 3, 8, 3, 9] = 2

removeAll :: Int -> [Int] -> [Int]
removeAll 3 [5, !!3, 8, 3, 9] = [5, 8, 9]

listMarks :: String -> [StudentMark] -> [Int]
listMarks "Joe" [("Joe", 45), ("Sam", 70), ("Joe", 52)] = [45,52]

prefix :: [Int] -> [Int] -> Bool

subSequence :: [Int] -> [Int] -> Bool