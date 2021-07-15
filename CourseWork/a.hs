
--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--



--
-- Types (define Place type here)
--
type City = String
type Long = Float
type Lat = Float
type Location = (Long, Lat)
type DRF = [Int]
type Place = (City, Location, DRF)

testData :: [Place]
testData = [("London", (51.5, -0.1), [0, 0, 5, 8, 8, 0, 0]), ("Cardiff", (51.5, -3.2), [12, 8, 15, 0, 0, 0, 2]), ("Norwich", (52.6, 1.3), [0, 6, 5, 0, 0, 0, 3]), ("Birmingham", (52.5, -1.9), [0, 2, 10, 7, 8, 2, 2]), ("Liverpool", (53.4, -3.0), [8, 16, 20, 3, 4, 9, 2]), ("Hull", (53.8, -0.3), [0, 6, 5, 0, 0, 0, 4]), ("Newcastle", (55.0, -1.6), [0, 0, 8, 3, 6, 7, 5]), ("Belfast", (54.6, -5.9),   [10, 18, 14, 0, 6, 5, 2]), ("Glasgow", (55.9, -4.3),   [7, 5, 3, 0, 6, 5, 0]), ("Plymouth", (50.4, -4.1),   [4, 9, 0, 0, 0, 6, 5]), ("Aberdeen", (57.1, -2.1),   [0, 0, 6, 5, 8, 2, 0]), ("Stornoway", (58.2, -6.4),   [15, 6, 15, 0, 0, 4, 2]), ("Lerwick", (60.2, -1.1),   [8, 10, 5, 5, 0, 0, 3]), ("St Helier", (49.2, -2.1),   [0, 0, 0, 0, 6, 10, 0])]
--
--  Your functional code goes here
--

first :: Place -> String
first (x,_,_) = x

second :: Place -> (Location)
second (_,x,_) = x

third :: Place -> [Int]
third (_,_,x) = x

names :: [Place] -> String
names [] = ""
names (x:xs) = first(x) ++ "\n" ++ names(xs)

selectPlace :: String -> [Place] -> Place
selectPlace name li = head [(x,y,z) | (x,y,z) <- li, name == x]


selectNameAndRainfall :: [Place] -> [(City, DRF)]
selectNameAndRainfall li = [(x,z) | (x,y,z) <- li]

printNameAndRainfall :: [(City, DRF)] -> String
printNameAndRainfall [] = ""
printNameAndRainfall ((x1,x2):xs) = formatText x1 ++ formatText(show(x2)) ++ "\n" ++ printNameAndRainfall(xs)

sumRainfall :: [Int] -> Int
sumRainfall [] = 0
sumRainfall (x:xs) = x + sumRainfall(xs)

averageRainfall :: Place -> Float
averageRainfall (x,y,z) = (fromIntegral(sumRainfall z)) / 7

printPlace :: Place -> String
printPlace (x,(y1,y2),z) = "Place: " ++ x ++ "\n" ++ "Longtitude: " ++ show(y1) ++ "\n" ++ "Latitude: " ++ show(y2) ++ "\n" ++ "Rainfall: " ++ arrayAsString z ++ "\n\n"

printPlaceArray :: [Place] -> String
printPlaceArray [] = ""
printPlaceArray ((x,(y1,y2),z):xs) = "Place: " ++ x ++ "\n" ++ "Longtitude: " ++ show(y1) ++ "\n" ++ "Latitude: " ++ show(y2) ++ "\n" ++ "Rainfall: " ++ arrayAsString z ++ "\n\n" ++ printPlaceArray(xs)


arrayAsString :: [Int] -> String
arrayAsString [] = ""
arrayAsString (x:xs) = show(x) ++ " " ++ arrayAsString(xs)

formatText :: String -> String
formatText str
    | length str <= 15 = formatText (str ++ " ")
    | otherwise = str

rainfallOnDay :: Int -> [Place] -> [String]
rainfallOnDay numDays li = [(x) | (x,y,z) <- li, getRainfallOnDay numDays z == 0]

placeRainfallOnDay :: Int -> [Place] -> [Place]
placeRainfallOnDay numDays li = [(x,y,z) | (x,y,z) <- li, getRainfallOnDay numDays z == 0]


getRainfallOnDay :: Int -> [Int] -> Int
getRainfallOnDay day rainfall = reverse rainfall !! day

printStringArray :: [String] -> String
printStringArray [] = ""
printStringArray (x:xs) = x ++ "\n" ++ printStringArray(xs)

changeRainfall :: [Int] -> [Place] -> [Place]
changeRainfall newValList places
    | length places > 0 = [changeValue 6 (head newValList) (head places)] ++ changeRainfall (tail newValList) (tail places)
    | otherwise = []


changeValue :: Int -> Int -> Place -> Place
changeValue n newVal (x,y,z) = (x,y, init z ++ [newVal])

changePlace :: City -> City -> Location -> DRF -> [Place] -> [Place]
changePlace scity ncity nlocation ndrf li
    | length li > 0 = [change (head li) scity ncity nlocation ndrf] ++ changePlace scity ncity nlocation ndrf (tail li)
    | otherwise = []

change :: Place -> City -> City -> Location -> DRF -> Place
change (ci, lo, drf) scity ncity nlocation ndrf
    | ci == scity = (ncity, nlocation, ndrf)
    | otherwise = (ci, lo, drf)

location :: Location -> [Place] -> [(City, Float)]
location (xa,ya) li = [(x,(distance (xa,ya) (xb,yb))) | (x,(xb, yb),z) <- li]

distance :: Location -> Location -> Float
distance (xa, ya) (xb, yb) = sqrt((xa - xb)^2 + (ya - yb)^2)

printNameAndLocation :: (City, Float) -> String
printNameAndLocation (x1,x2) = "City: " ++ show(x1) ++ "\nDistance from given co-ords: " ++ show(x2)

-- averageRainfall :: Place -> float


--function in your program.

--  Demo
--

demo :: Int -> IO ()
demo 1 = putStrLn(names testData)
demo 2 = putStrLn(show(averageRainfall (selectPlace "London" testData)))-- display, to two decimal places, the average rainfall in Cardiff
demo 3 = putStrLn(printNameAndRainfall (selectNameAndRainfall testData))
demo 4 = putStrLn(printStringArray(rainfallOnDay 3 testData))
demo 5 = putStrLn(printPlaceArray (changeRainfall [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData) )
demo 6 = putStrLn(printPlaceArray (changePlace "Plymouth" "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1] testData))
demo 7 = putStrLn(printNameAndLocation(head((location (50.9, -1.3) (placeRainfallOnDay 1 (changePlace "Plymouth" "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1] testData))))))

-- demo 8 = -- display the rainfall map

main :: IO ()
main = demo 5
--
-- Screen Utilities (use these to do the rainfall map - note that these do
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text


--
-- Your rainfall map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--


main :: IO ()
main = do
    contents <- readFile "PlaceData.txt"
    putStrLn("\n\n -- I/O MENU -- \n\nPlease select an option by entering the corresponding number below\n\n1)View the names of all the cities\n2)View the average rainfall of a given city\n3)View the rainfall figures for all the cities\n4)View the cities that were dry a given number of days ago\n5)Update the most current rainfall value for each city\n6)Replace a given existing place with a new place\n7)View the name and location of the closest place that was totally dry\n\n")
    opt <- getLine
    if (opt == "") then do
        return ()
    else
        if (opt == "1") then do
            let li = (toList contents) in
                putStr ("\n" ++ names li ++ "\n")
            main
        else
            if (opt == "2") then do
                putStr("\nEnter a city: ")
                city <- getLine
                let li = (toList contents) in
                    putStr(show(averageRainfall (selectPlace city li)) ++ "\n\n")
                main
            else
                if (opt == "3") then do
                    let li = (toList contents) in
                        putStr(printNameAndRainfall (selectNameAndRainfall li))
                    main
                else
                    if (opt == "4") then do
                        putStr("Number of days ago: ")
                        days <- getLine
                        let li = (toList contents) in
                            putStr("\n" ++ printStringArray(rainfallOnDay (read days :: Int) li))
                        main
                    else
                        if (opt == "5") then do
                            putStr ("New rainfall values: ")
                            newRainfall <- getLine
                            let li = (toList contents) in
                                putStr(printPlaceArray (changeRainfall (read newRainfall :: [Int]) li))
                            contents <- putStrLn(printPlaceArray (changeRainfall (read newRainfall :: [Int]) (toList contents)))
                            main
                        else
                            if (opt == "6") then do
                                putStr("Enter city to replace: ")
                                newCity <- getLine
                                putStr("Enter name of new city: ")
                                newName <- getLine
                                putStr("Enter a new location: ")
                                newLocation <- getLine
                                putStr("Enter new rainfall values: ")
                                newRainfallVal <- getLine
                                let li = (toList contents) in
                                    putStr(printPlaceArray (changePlace newCity newName (read newLocation :: Location) (read newRainfallVal :: [Int]) li))
                                main
                            else
                                if (opt == "7") then do
                                    putStr("Enter a location: ")
                                    loc <- getLine
                                    let li = (toList contents) in
                                        putStr(printNameAndLocation(head((location (read loc :: Location) li))))
                                    main
                                else
                                    if (opt == "8") then do
                                        putStr("Save? \n1) Yes\n2)No\n\n")
                                        saveOpt <- readLn
                                        if (saveOpt == "1") then do
                                            putStr("Please enter the name you wish to give to your save: ")
                                            saveName <- readLn
                                            writeFile (saveName ++ ".txt") (show (toList contents))
                                            main
                                        else
                                            if (saveOpt == "2") then do
                                                putStr("Okay")
                                                main
                                            else
                                                main
                                    else do
                                        putStrLn("\nPlease enter a valid option\n")
                                        main
toList :: String -> [Place]
toList = read