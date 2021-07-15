{-
    MATHFUN
    UP853829
    Haskell - RainFall Average -- Course Work
-}

import Data.List
import System.IO
import System.Exit (exitSuccess) -- Allows For A True System Quit When Users Chooses `q`
import Text.Printf (printf) -- For Padding Functions

{-
    Defining Types
    Place: City - Lat & Long - RainFall Averages as Array
-}
type City = String
type Coords = (Float, Float)
type RainFall = [Int]
type Place = (City, Coords, RainFall)

-- Same data as places.txt
testData :: [Place]
testData =
    [
        ("London", (51.0, -0.1), [0, 0, 5, 8, 8, 0, 0]),
        ("Cardiff", (51.5, -3.2), [12, 8, 15, 0, 0, 0, 2]),
        ("Norwich", (52.6, 1.3), [0, 6, 5, 0, 0, 0, 3]),
        ("Birmingham", (52.5, -1.9), [0, 2, 10, 7, 8, 2, 2]),
        ("Liverpool", (53.4, -3.0), [8, 16, 20, 3, 4, 9, 2]),
        ("Hull", (53.8, -0.3), [0, 6, 5, 0, 0, 0, 4]),
        ("Newcastle", (55.0, -1.6), [0, 0, 8, 3, 6, 7, 5]),
        ("Belfast", (54.6, -5.9), [10, 18, 14, 0, 6, 5, 2]),
        ("Glasgow", (55.9, -4.3), [7, 5, 3, 0, 6, 5, 0]),
        ("Plymouth", (50.4, -4.1), [4, 9, 0, 0, 0, 6, 5]),
        ("Aberdeen", (57.1, -2.1), [0, 0, 6, 5, 8, 2, 0]),
        ("Stornoway", (58.2, -6.4), [15, 6, 15, 0, 0, 4, 2]),
        ("Lerwick", (60.2, -1.1), [8, 10, 5, 5, 0, 0, 3]),
        ("St Helier", (49.2, -2.1), [0, 0, 0, 0, 6, 1, 0])
    ]

{-
    Functional Code - Core
-}

{-
    Question 1:
    return a list of the names of all the places
-}
getName :: [Place] -> String
getName [] = "" -- Empty String
getName (city:cities) = "Location: " ++ getCity(city) ++ "\n" ++ getName(cities)
    where
        getCity :: Place -> String
        getCity (city, _, _) = city

{-
    Question 2:
    return the average rainfall (as a float) for a place given its name
-}
searchForCity :: String -> [Place] -> Place
searchForCity desiredCity rainfallData = head [(city, coords, averages) | (city, coords, averages) <- rainfallData, desiredCity == city]

averageRainfall :: Place -> Float
averageRainfall (_, _, averages) = twoDecPoint'((fromIntegral(foldl (+) 0 averages )) / 7) 2 -- I used `foldl` over `sum` for performance
    where
        -- Alternative to `PrintF %.2f` as `Ambiguous type variable ‘t0’ arising from a use of ‘fromIntegral’`
        twoDecPoint' :: Float -> Int -> Float
        twoDecPoint' value indices = (fromIntegral (floor (value * 10^indices))) / (10^indices)
{-
    Question 3:
    return all place names and their 7-day rainfall figures
-}
---------------------- For columns -------------------------
padL :: a -> Int -> [a] -> [a]
padL pad string len
    | length len >= string = len
    | otherwise = replicate (string - length len) pad ++ len

padR :: a -> Int -> [a] -> [a]
padR pad string len = take string $ len ++ repeat pad
------------------------------------------------------------

searchForCityRainFall :: [Place] -> [(City, RainFall)]
searchForCityRainFall rainfallData = [(city, averages) | (city, coords, averages) <- rainfallData]

convertAveragesColumn :: [Int] -> String
convertAveragesColumn [] = ""  -- Empty String
convertAveragesColumn (average:averages) = show(average) ++ "  |  " ++ convertAveragesColumn(averages)

cityRainFall :: [(City, RainFall)] -> String
cityRainFall [] = "" -- Empty String
cityRainFall ((city, averages):cities) = "Location and RainFall:  | " ++ prettify(city) ++ " | " ++ convertAveragesColumn(averages) ++ "\n" ++ cityRainFall(cities)
    where
        -- Able to Pad City Column But Does Not Work With The RainFall Array
        prettify :: String -> String
        prettify formatted
             | length formatted <= 10 = prettify (formatted ++ " ")
             | otherwise = formatted

{-
    Question 4:
    return a list of the names of places that were totally dry
-}
convertPlaceArray :: [String] -> String
convertPlaceArray [] = ""
convertPlaceArray (city : cities) = "Location with 0 rainfall for 2 days: " ++ city ++ "\n" ++ convertPlaceArray(cities)

-- Used for 4 and 7
getDailyAverage :: Int -> [Int] -> Int
getDailyAverage daily averages = reverse averages !! daily

dailyRainFallCity :: Int -> [Place] -> [String]
dailyRainFallCity daily rainfallData = [(city) | (city, coords, averages) <- rainfallData, getDailyAverage daily averages == 0]

-- Used for 5 and 6
formatCityInfo :: [Place] -> String
formatCityInfo [] = "" -- Empty String
formatCityInfo ((city, (lat, long), averages):cities) = "Location: " ++ city ++ ": " ++ "Lat: " ++ show(lat) ++ " - " ++ "Long: " ++ show(long) ++ "\n" ++ "Daily Rainfall: " ++ convertAverages(averages) ++ "\n\n" ++ formatCityInfo(cities)

{-
    Question 5:
    update the data given a list of most recent rainfall figures
-}
convertAverages :: [Int] -> String
convertAverages [] = ""  -- Empty String
convertAverages (city:cities) = show(city) ++ " " ++ convertAverages(cities)

updateRain :: [Int] -> [Place] -> [Place]
updateRain updatedRainFallData cities
    | length cities > 0 = [updateRainInt 6 (head updatedRainFallData) (head cities)] ++ updateRain (tail updatedRainFallData) (tail cities)
    | otherwise = [] -- Values Are Same
    where
        updateRainInt :: Int -> Int -> Place -> Place
        updateRainInt currentInt newInt (city, coords, averages) = (city, coords, init averages ++ [newInt])

{-
    Question 6:
    replace a given existing place with a new place
-}
updateCity :: City -> City -> Coords -> RainFall -> [Place] -> [Place]
updateCity city updatedCity updatedCoords updatedAverages cityData
    | length cityData > 0 = [updatePlace (head cityData) city updatedCity updatedCoords updatedAverages] ++ updateCity city updatedCity updatedCoords updatedAverages (tail cityData)
    | otherwise = [] -- Values Are Same
    where
        updatePlace :: Place -> City -> City -> Coords -> RainFall -> Place
        updatePlace (city, coords, averages) currentCity updatedCity updatedCoords updatedAverages
            | city == currentCity = (updatedCity, updatedCoords, updatedAverages)
            | otherwise = (city, coords, averages) --Keep Original Values Same -- Tried To Parse Place

{-
    Question 7:
    given a location return the closest place that was totally dry yesterday
-}
getNearestCity :: Coords -> [Place] -> [(City, Float)]
getNearestCity (lat1, long1) placeData = [(city, (pythagorasTheoremOfDistance (lat1 ,long1) (lat2, long2))) | (city, (lat2, long2), averages) <- placeData]
    -- Nested Assignment with Nested Values For Ease Of Use And Grouping of Task Functionality
    where
    pythagorasTheoremOfDistance (x1 , y1) (x2 , y2) = sqrt(x'*x' + y'*y')
        where
            x' = x1 - x2
            y' = y1 - y2

dailyRainFallPlace :: Int -> [Place] -> [Place]
dailyRainFallPlace daily rainfallData = [(city, coords, average) | (city, coords, average) <- rainfallData, getDailyAverage daily average == 0]

getCityLocation :: (City, Float) -> String
getCityLocation (city, coords) = "Given City: " ++ city ++ "\nThe Closest Place That Was Totally Dry Yesterday - City Cords: " ++ show(coords)

{-
    Demo Functions
    Used To Test And Run Core Functionality
-}
demo :: Int -> IO ()
demo 1 = putStrLn(getName testData)
demo 2 = putStrLn("Average RainFall for Selected City: " ++ show(averageRainfall (searchForCity "Cardiff" testData)))
demo 3 = putStrLn("6 Wonky Columns \n" ++ cityRainFall (searchForCityRainFall testData))
demo 4 = putStrLn(convertPlaceArray(dailyRainFallCity 2 testData))
demo 5 = putStrLn(formatCityInfo(updateRain [0, 8, 0, 0, 5, 0, 0, 3, 4, 2, 0, 8, 0, 0] testData))
demo 6 = putStrLn(formatCityInfo(updateCity "Plymouth" "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1] testData))
demo 7 = putStrLn(getCityLocation(head((getNearestCity (50.9, -1.3) (dailyRainFallPlace 1 (updateCity "Plymouth" "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1] testData))))))
-- demo 8 = display the rainfall map

{-
    Screen Utilities (use these to do the rainfall map - note that these do
    not work in WinGHCi on Windows, so use GHCi.)
-}
type ScreenPosition = (Int, Int)

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
{-
    RainFall Map - N/A
-}

{-
    Main IO
    Your user interface (and loading/saving) code goes here
-}

-- Converts User Input -> Return Into a Place Type
convertInputPlace :: String -> [Place]
convertInputPlace = read

{-
    Main IO
    Your user interface (and loading/saving) code goes here
-}
-- Use Me If Something Breaks -- Used For Testing And Running Each Demo Individually - UI/IO Should Be Working
main :: IO ()
main = demo 1

-- main :: IO ()
-- main = do
--     tempDatabase <- readFile "places.txt" -- Backup To Remove `let tempDatabase` As I Was Having Trouble Reading File

--     putStrLn("-------------------------------------------------------------------------------\n")
--     putStrLn("                    MATHFUN Haskell Course Work - RainFall Averages            \n")
--     putStrLn("                              Student: UP853829                                \n")
--     putStrLn("-------------------------------------------------------------------------------\n")

--     -- Print Possible Options
--     putStrLn("Please Select An Input From The Following Options: ")
--     putStrLn("1 : Display Current Cities")
--     putStrLn("2 : Display Average RainFall For Particular City")
--     putStrLn("3 : Display Cities and RainFall As Table")
--     putStrLn("4 : Display A Dry Period For A City")
--     putStrLn("5 : Update A City's RainFall Data")
--     putStrLn("6 : Update List Of Cities")
--     putStrLn("7 : Display The Closest Dry City ")
--     putStrLn("8 : Save Progress")
--     putStrLn("q : Quit the application")

--     usr_In <- getLine

--     if(usr_In == "1") then do
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr("\n" ++ getName placeData ++ "\n")
--         main
--     else
--         return ()

--     if(usr_In == "2") then do
--         putStr("Enter Desired City: ")
--         desiredCity <- getLine
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(show(averageRainfall (searchForCity desiredCity placeData)) ++ "\n\n")
--         main
--     else
--         return ()

--     if(usr_In == "3") then do
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(cityRainFall (searchForCityRainFall placeData))
--         main
--     else
--         return ()

--     if(usr_In == "4") then do
--         putStr("Enter The Desired Days: ")
--         daily <- getLine
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(convertPlaceArray(dailyRainFallCity (read daily) placeData))
--         main
--     else
--         return ()

--     if(usr_In == "5") then do
--         putStr("Enter New RainFall Average Data: ")
--         newRainfall <- getLine
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(formatCityInfo (updateRain (read newRainfall) placeData)) -- Read Custom Input
--         tempDatabase <- putStrLn(formatCityInfo (updateRain (read newRainfall) (convertInputPlace tempDatabase))) -- Read Custom Input
--         main
--     else
--         return ()

--     if(usr_In == "6") then do
--         putStr("Enter Current City To Be Updated: ")
--         desiredCity <- getLine

--         putStr("Enter Desired City: ")
--         desiredName <- getLine

--         putStr("Enter Desired Location - Corrds: ")
--         desiredLocation <- getLine

--         putStr("Enter Desired RainFall Averages: ")
--         updatedRainfall <- getLine

--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(formatCityInfo (updateCity desiredCity desiredName (read desiredLocation) (read updatedRainfall) placeData))
--         main
--     else
--         return ()

--     if(usr_In == "7") then do
--         putStr("Enter Desired City: ")
--         desiredCity <- getLine
--         let placeData = (convertInputPlace tempDatabase) in
--             putStr(getCityLocation(head((getNearestCity (read desiredCity) placeData))))
--         main
--     else
--         return ()

--     if(usr_In == "8") then do
--         putStr("Do You Wish To Save Your Changes? : ")
--         putStr("1 - Yes")
--         putStr("2 - No")
--         usr_In_Save <- readLn
--         if (usr_In_Save == "1") then do
--             writeFile "places.txt" (show(convertInputPlace tempDatabase))
--             main
--         else
--             if (usr_In_Save == "2") then do
--                 putStrLn("Thank you For Using")
--                 main
--             else
--                 main
--     else
--         return ()

--     if(usr_In == "q") then do
--         exitSuccess
--     else
--         return ()