--  New version of absolute from week 1
absolute :: Int -> Int
absolute n =
    | n < 0 = -n
    | otherwise = n

sign :: Int -> Int
sign n
    | n > 0 = 1
    | n < 0 = -1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | (x /= y && y /= z && x /= z) = 0
    | (x == y || y == z || x == z) = 2
    | (x == y && y == z && x == z) = 3

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = dx + dy + dz
    where
        dx = sqrt(x^2 + x^2)
        dy = sqrt(y^2 + y^2)
        dz = sqrt(z^2 + z^2)

taxiFare :: Int -> Float
taxiFare k
        where fare = 2.20
        | if k >= 10 then fare + 50 else fare + 30


howManyAboveAverage :: Int -> Int -> Int -> Int

validDate :: Int -> Int -> Bool

daysInMonth :: Int -> Int -> Int