
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type
data Shape = Circle Float |
             Rectangle Float Float
             deriving (Show)

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String |
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null |
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- worksheet
-- Enumerated Types

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sept | Oct | Nov | Dec
  deriving (Eq, Ord)

data Season = Spring | Summer | Autumn | Winter
  deriving (Show)

-- Spring: 20 Mar
-- Summer: 21 Jun
-- Autumn: 23 Sept
-- Winter 21 Dec
season :: Month -> Season
season month
  | month == Dec = Winter
  | month >= Sept = Autumn
  | month >= Jun = Summer
  | month >= Mar = Spring
  | month >= Jan = Winter

numberOfDays :: Month -> Int -> Int
numberOfDays Feb year = if (year `mod` 4 == 0) then 29 else 28
numberOfDays Apr year = 30
numberOfDays Jun year = 30
numberOfDays Sept year = 30
numberOfDays _ year = 31

-- Points and Shapes
data Point = Point Float Float
  deriving (Show)

data PositionedShape = PositionedShape Shape Point
  deriving (Show)

circPositionedShape = PositionedShape (Circle 2.0) (Point 4.0 5.0)
rectPositionedShape = PositionedShape (Rectangle 3 4) (Point 5 5)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape (Circle r) (Point x y)) dx dy = (PositionedShape (Circle r) (Point (x + dx) (y + dy)))
move (PositionedShape (Rectangle h w) (Point x y)) dx dy = (PositionedShape (Rectangle h w) (Point (x + dx) (y + dy)))

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ st1 st2) = 1 + numberOfNodes st1 + numberOfNodes st2

isMember :: Int -> Tree -> Bool
isMember n Null = False
isMember n (Node n' st1 st2) = (n == n') || isMember n st1 || isMember n st2

leaves :: Tree -> [Int]
leaves Null = []
leaves (Node n Null Null) = n : []
leaves (Node _ st1 st2) = leaves st1 ++ leaves st2

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n Null Null) = n : []
inOrder (Node n st1 st2) = inOrder st1 ++ [n] ++ inOrder st2

insert :: Int -> Tree -> Tree
insert n Null = (Node n Null Null)
insert n (Node n' st1 st2)
  | n == n' = Node n' st1 st2
  | n < n' = Node n' (insert n st1) st2
  | n > n' = Node n' st1 (insert n st2)
