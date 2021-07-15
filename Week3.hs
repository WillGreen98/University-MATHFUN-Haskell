-- Weeksheet 3 selected model solutions

import Prelude hiding ((&&), gcd)

--
infixr 3 &&

-- Supplied code for || omitted

-- 1
(&&) :: Bool -> Bool -> Bool
True && True    = True
False && True   = False
True && False   = False
False && False  = False

-- Alternative version
-- True && True = True
-- _ && _       = False

-- Another alternative
-- False && _     =  False
-- True && a      = a

-- 2
exor :: Bool -> Bool -> Bool
exor False False = False
exor True True   = False
exor _ _         = True

-- 5
sumNumbers :: Int -> Int
sumNumbers n
   | n == 0       = 0
   | n > 0        = n + sumNumbers (n - 1)

-- 7
power :: Int -> Int -> Int
power x y
   | y == 0       = 1
   | y > 0        = x * power x (y - 1)

-- 9
gcd :: Int -> Int -> Int
gcd a b
    | a == b        = a
    | a > b         = gcd (a - b) b
    | otherwise     = gcd (b - a) a
