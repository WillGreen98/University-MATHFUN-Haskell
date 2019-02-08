import Prelude hiding ((||))

infixr 2  ||

-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- False || True = True
-- True || False = True
-- False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False = False
--_ || _ = True

-- Another alternative re-implementation
(||) :: Bool -> Bool -> Bool
True || _ =  True
False || a = a

fact :: Int -> Int
fact n
    | n == 0 = 1
    | n > 0 = n * fact(n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
    | n == 0 = 0
    | n > 0 = m + mult(n - 1) m

divide :: Int -> Int -> Int
divide n m
    | n < m = 0
    | otherwise = 1 + divide(n - m) m
