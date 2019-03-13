{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit

addUp :: Num a => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []
