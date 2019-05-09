{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit
keepEvens = filter isEven


addUp :: Num a => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- worksheet

-- use of map, filter and foldr
mult10 = map (*10)

onlyLowerCase = filter isLower

-- this function finds the disjunction (or) of the elements in a Boolean list
orAll bs = foldr (||) False bs

sumSquares = addUp . map(^2)

zeroToTen = filter (>=0) . filter (<=10)

squareRoots = map (sqrt) . filter (>0)

countBetween x y = length . filter(>=x) . filter(<=y)

alwaysPositive f (x:xs) = if (foldr (+) 0 (map f (x:xs))) > 0 then True else False

productSquareRoots (x:xs) = foldr (*) 1 (filter (>0) (map sqrt (x:xs)))

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst p (x:xs)
    | not (p x) = x : removeFirst p xs
    | otherwise = xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast _ [] = []
removeLast p xs
    | not (p (last xs)) = removeLast p (init xs) ++ [last xs]
    | otherwise = init xs

zeroToTenLambda = filter (\x -> x>=0 && x<=10)
