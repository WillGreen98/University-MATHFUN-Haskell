{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

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

type StudentMark = (String, Int)

-- worksheet

-- list patterns
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = head (x:xs) + 1

duplicateHead :: [a] -> [a]
duplicateHead []  = []
duplicateHead (x:xs) = head (x:xs) : (x:xs)

rotate :: [a] -> [a]
rotate (x:xs) = if length (x:xs) < 2 then (x:xs) else head xs : x : tail xs ++ []

-- recursion over lists
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems i (x:xs)
  | i == x = 1 + countElems i xs
  | otherwise = countElems i xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll i (x:xs)
  | i == x = removeAll x xs
  | otherwise = x : removeAll i xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks std (x:xs)
  | std == fst x = snd x : listMarks std xs
  | otherwise = listMarks std xs

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (x:xs) (y:ys)
  | x /= y = False
  | otherwise = prefix xs ys

subSequence :: [Int] -> [Int] -> Bool
subSequence xs (y:ys) = prefix xs (y:ys) || subSequence xs ys
