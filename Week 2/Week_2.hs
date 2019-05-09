absolute :: Int -> Int
absolute n
  | n < 0 = n * (-1)
  | otherwise = n

sign :: Int -> Int
sign n
  | n > 0 = 1
  | n < 0 = -1
  | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z = 3
  | x == y || x == z || y == z = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float -- length of diagonal of a square = x√2
sumDiagonalLengths s1 s2 s3 =
  sum * sqrt 2
  where sum = s1 + s2 + s3

taxiFare :: Int -> Float
taxiFare km
  | km < 0 = 0
  | km <= 10 = intialFare + (fromIntegral km * 0.50)
  | otherwise = taxiFare 10 + fromIntegral km * 0.30 - 3
  where intialFare = 2.20

howManyAboveAverage :: Int -> Int -> Int -> Int -- only two numbers (in max) can be above their average
howManyAboveAverage x y z
  | (fromIntegral x > average && fromIntegral y > average) || (fromIntegral x > average && fromIntegral z > average) || (fromIntegral y > average && fromIntegral z > average) = 2
  | fromIntegral x > average || fromIntegral y > average || fromIntegral z > average = 1
  | otherwise = 0
  where average = fromIntegral (x + y + z) / 3

validDate :: Int -> Int -> Bool
validDate day month
  | (day > 28 && month == 2) || (day > 30 && month == 4) || (day > 30 && month == 6) || (day > 30 && month == 9) || (day > 30 && month == 11) = False
  | (day > 0 && day < 32) && (month > 0 && month < 13) = True
  | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 && year `mod` 4 == 0 = 29
  | month == 2 = 28
  | month == 4 || month == 6 || month == 9 || month == 11 = 30
  | otherwise = 31
