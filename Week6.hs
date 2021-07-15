-- Worksheet 6 selected model solutions

import Data.Char

-- 1
mult10 :: [Int] -> [Int]
mult10 = map (*10)

-- 2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- 3
orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- 4 
sumSquares :: [Int] -> Int
sumSquares = foldr (+) 0 . map (^2) 

-- 5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>=0) . filter (<=10) 

-- 6 
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>=0)
