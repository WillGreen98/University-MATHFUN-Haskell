-- Worksheet 7 selected model solutions

--1
data Month = Jan | Feb | Mar | Apr | May | Jun | 
             Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Eq,Ord,Show)

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq,Show)

-- 2
season :: Month -> Season
season month 
    | month == Dec    = Winter
    | month <= Feb    = Winter
    | month <= May    = Spring
    | month <= Aug    = Summer
    | otherwise       = Autumn

-- 3
numberOfDays :: Month -> Int -> Int
numberOfDays m y
    | m == Apr || m == Jun || m == Sep || m == Nov    = 30
    | m == Feb && y `mod` 4 == 0                      = 29
    | m == Feb                                        = 28
    | otherwise                                       = 31


-- 4
data Point = Point Float Float
             deriving (Eq,Show)

-- 5 
data PositionedShape = Circle Point Float |
                       Rectangle Point Float Float
                       deriving (Eq,Show)

-- 6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
move (Rectangle (Point x y) h w) dx dy = Rectangle (Point (x+dx) (y+dy)) h w

