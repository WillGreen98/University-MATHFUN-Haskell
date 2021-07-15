-- Worksheet 1 selected model solutions

-- Ex 1
timesTen :: Int -> Int
timesTen n = n * 10

-- Ex 2
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Ex 3
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

-- Ex 4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder length radius = areaOfCircle radius * length

-- Ex 5
distance ::  Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

-- Ex 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z
