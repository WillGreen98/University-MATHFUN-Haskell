-- Worksheet 2 selected model solutions

-- Ex 1
absolute :: Int -> Int
absolute a 
    | a > 0     = a
    | otherwise = -a

-- Ex 3 
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x /= y && x /= z && y /= z  = 0
    | x == y && x == z            = 3
    | otherwise                   = 2

-- Ex 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z 
    = diagonal x + diagonal y + diagonal z
      where
      diagonal s = sqrt (2 * s^2)

-- Ex 6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    | howManyEqual x y z == 3     = 0
    | x > average && y > average  = 2
    | y > average && z > average  = 2
    | x > average && z > average  = 2
    | otherwise                   = 1
    where average = div (x + y + z) 3

-- Ex 8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 && year `mod` 4 == 0                       = 29
    | month == 2                                            = 28
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | otherwise                                             = 31

