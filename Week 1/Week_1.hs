timesTen :: Int -> Int
timesTen x = x * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle radius = radius * pi^2

volumeOfCylinter :: Float -> Float -> Float
volumeOfCylinter radius height = areaOfCircle radius * height

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `rem` y == 0

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3

absolute :: Int -> Int
absolute n = if n >= 0 then n else -n