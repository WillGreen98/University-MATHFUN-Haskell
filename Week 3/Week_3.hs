-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2  ||
infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

-- A naive re-implementation of the Prelude opreator &&
(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = False
True && False = False
False && False = False

-- An alternative re-implementation
-- (&&) :: Bool -> Bool -> Bool
-- True && a = a
-- a && _ = a


fact :: Int -> Int
fact n
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

-- Pattern Matching
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse condition n1 n2 = if condition then n1 else n2

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth month = if (month == 4 || month == 6 || month == 9 || month == 11) then 30 else 31

-- Recursion
sumNumbers :: Int -> Int
sumNumbers n
  | n == 0 = 0
  | n > 0 = n + sumNumbers(n - 1)
  | otherwise = error "undefined for neg ints"

sumSquares :: Int -> Int
sumSquares n
  | n == 0 = 0
  | n > 0 = (n ^ 2) + sumSquares((n - 1))

power :: Int -> Int -> Int
power base 0 = 1
power base index
  | index == 1 = base
  | index > 0 = base * power base (index - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | x > y = 0
  | x == y = x
  | x < y = y + sumFromTo x (y - 1)

gcd :: Int -> Int -> Int
gcd x y
  | x == y = x
  | otherwise = gcd (abs (x - y)) (min x y)

intSquareRoot :: Int -> Int
intSquareRoot n = floor (sqrt (fromIntegral n))
