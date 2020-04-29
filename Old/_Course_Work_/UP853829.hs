-- MATHFUN
-- Functional Programming Haskell Assignment Program - Albums
-- UP853829

import Data.Map as M
import Data.List as L

import Control.Monad (when)
import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering), hReady)

-- Album Data
data Album = Album { title :: String
                ,    artist :: String
                ,    year :: Int
                ,    totalSales :: Int
        } deriving (Eq, Show, Read)

-- Testing Data
testData :: [Album]
testData = [Album "Greatest Hits"  "Queen"  1981  6300000,
            Album "Gold: Greatest Hits"  "ABBA" 1992  5400000,
            Album "Sgt. Pepper's Lonely Hearts Club Band"  "The Beatles"  1967  5340000,
            Album "21" "Adele"  2011  5110000,
            Album "(What's the Story) Morning Glory?"  "Oasis"  1995  4940000,
            Album "Thriller" "Michael Jackson"  1982  4470000,
            Album "The Dark Side of the Moon"  "Pink Floyd" 1973  4470000,
            Album "Brothers in Arms" "Dire Straits" 1985  4350000,
            Album "Bad"  "Michael Jackson"  1987  4140000,
            Album "Rumours"  "Fleetwood Mac"  1977  4090000,
            Album "Greatest Hits II" "Queen"  1991  3990000,
            Album "Back to Black"  "Amy Winehouse"  2006  3940000,
            Album "The Immaculate Collection"  "Madonna"  1990  3700000,
            Album "25" "Adele"  2015  3500000,
            Album "Stars"  "Simply Red" 1991  3450000,
            Album "Come On Over" "Shania Twain" 1998  3430000,
            Album "x"  "Ed Sheeran" 2014  3380000,
            Album "Legend" "Bob Marley" 1984  3380000,
            Album "Bat Out of Hell"  "Meat Loaf"  1977  3370000,
            Album "Back to Bedlam" "James Blunt"  2004  3360000,
            Album "Urban Hymns"  "The Verve"  1997  3340000,
            Album "Bridge over Troubled Water" "Simon & Garfunkel"  1970  3260000,
            Album "1"  "The Beatles"  2000  3230000,
            Album "Spirit" "Leona Lewis"  2007  3170000,
            Album "Crazy Love" "Michael Bublé"  2009  3130000,
            Album "No Angel" "Dido" 2000  3090000,
            Album "White Ladder" "David Gray" 1998  3020000,
            Album "The Fame" "Lady Gaga"  2009  2990000,
            Album "Only by the Night"  "Kings of Leon"  2008  2980000,
            Album "A Rush of Blood to the Head"  "Coldplay" 2002  2960000,
            Album "Talk on Corners"  "The Corrs"  1997  2960000,
            Album "Spice"  "Spice Girls"  1996  2960000,
            Album "Life for Rent"  "Dido" 2003  2900000,
            Album "Beautiful World"  "Take That"  2006  2880000,
            Album "The Joshua Tree"  "U2" 1987  2880000,
            Album "Hopes and Fears"  "Keane"  2004  2860000,
            Album "The War of the Worlds"  "Jeff Wayne" 1978  2800000,
            Album "X&Y"  "Coldplay" 2005  2790000,
            Album "Jagged Little Pill" "Alanis Morissette"  1995  2780000,
            Album "Tubular Bells"  "Mike Oldfield"  1973  2760000,
            Album "Scissor Sisters"  "Scissor Sisters"  2004  2760000,
            Album "...But Seriously" "Phil Collins" 1989  2750000,
            Album "Tracy Chapman"  "Tracy Chapman"  1988  2710000,
            Album "Parachutes" "Coldplay" 2000  2710000,
            Album "The Man Who"  "Travis" 1999  2687500,
            Album "Greatest Hits"  "ABBA" 1975  2606000,
            Album "I've Been Expecting You"  "Robbie Williams"  1998  2586500,
            Album "Come Away with Me"  "Norah Jones"  2002  2556650,
            Album "Graceland"  "Paul Simon" 1986  2500000,
            Album "Ladies & Gentlemen: The Best of"  "George Michael" 1998  250000]

-- Functional Code
addAlbumDescriptive :: [Album] -> String -> String -> Int -> Int -> [Album]
addAlbumDescriptive albumDB titled artistd yeard totalSalesd = albumDB ++ [Album titled artistd yeard totalSalesd]

albumsToString :: [Album] -> String
albumsToString [] = [] -- Needed?
albumsToString (Album t a y ts : albums) = "\n " ++ t ++ " " ++ a ++ " " ++ show y ++ " " ++ show ts ++ "\n" ++ albumsToString albums

albumsToStringOccurance :: [Album] -> String
albumsToStringOccurance [] = ""
albumsToStringOccurance (Album a _ _ _ : xs) = "Artist: " ++ a ++ "Occurrences: " ++ show xs ++ "\n" ++ albumsToStringOccurance xs

top :: [Album] -> Int -> [Album]
top _ 0 = [] -- Needed?
top (x : xs) counter = x : top xs (counter - 1)

betweenTwoYears :: [Album] -> Int -> Int -> [Album]
betweenTwoYears albums start end = L.filter (\(Album _ _ y _) -> start <= y && y <= end) albums

startsWith :: String -> [Album] -> [Album]
startsWith prefix albums = [Album t a y ts | (Album t a y ts) <- albums, prefix `L.isPrefixOf` t]

totalSalesU :: String -> [Album] -> [Int]
totalSalesU artistu albums = [ts | (Album _ a _ ts) <- albums, artistu == a]

displayArtist :: Char -> [Album] -> [Album]
displayArtist artistd = L.filter (\(Album _ a _ _) -> elem artistd a)

-- artistPairs :: [Album] -> [(String,Int)]
-- artistPairs albumsList = pairsList albumsList []
--     where pairsList [] _ = []
--           pairsList (Album _ artist _ _ : albumsList) done
--               | artist `elem` done = pairsList albumsList done
--               | otherwise = (artist , count artist ) : pairsList albumsList (artist:done)
--           count givenArtist = sum [1 | a <- L.map (\album@(Album _ artist _ _) -> artist) albumsList, a == givenArtist]

occurrences :: (Ord k, Num a) => [k] -> [(k, a)]
occurrences albums = M.toList (M.fromListWith (+) [(o, 1) | o <- albums])

updateNthElement :: Album -> Int -> [Album] -> [Album]
updateNthElement _ _ [] = []
updateNthElement newAlbum i (x : xs)
        | i == 0 = newAlbum : xs
        | otherwise = x : updateNthElement newAlbum (i - 1) xs -- Use of xs !! i

updateElement :: Album -> String -> Album -> Album
updateElement album titleu (Album t a y ts)
        | titleu == t = album
        | otherwise = Album t a y ts

updateAlbum :: Album -> String -> [Album] -> [Album]
updateAlbum album titleu albums = [updateElement album titleu (Album t a y ts)
        | (Album t a y ts) <- albums]

updateTotalSales :: String -> Int -> [Album] -> Album
updateTotalSales titles amount albums = head [Album t a y (ts + amount)
        | (Album t a y ts) <- albums, titles == t]

-- Demo Functions
demo :: Int -> IO ()
demo 1 = putStrLn (albumsToString testData)
demo 2 = putStrLn (albumsToString (top testData 10))
demo 3 = putStrLn (albumsToString (betweenTwoYears testData 2000 2008))
demo 4 = putStrLn (albumsToString (startsWith "Th" testData))
demo 5 = print (sum (totalSalesU "Queen" testData))
-- demo 6 = putStrLn (albumsToStringOccurance (top (occurrences (displayArtist (testData))) 50))
demo 7 = putStrLn (albumsToString (updateNthElement (Album "Progress" "Take That" 2010 2700000) (L.length testData - 1) testData))
demo 8 = putStrLn (albumsToString (updateAlbum (updateTotalSales "21" 400000 testData) "21" testData))
demo _ = putStrLn "Invalid Demo Request"

-- loadDB :: IO ()
-- loadDB = do
--         tempDatabase <- readFile "albums.txt" --
--         let database = read tempDatabase --
--         database <- main (database) --
--         writeFile "albums.txt" (show database) --
--         putStrLn "-------------------------------------------------------------------------------"
--         putStrLn "                                Load Successful"
--         putStrLn "-------------------------------------------------------------------------------\n"

saveDB :: [Album] -> IO ()
saveDB albumFormattedDB = do
        writeFile "albums_formatted.txt" (show albumFormattedDB)
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "                                Save Successful"
        putStrLn "-------------------------------------------------------------------------------\n"

getKey :: IO String
getKey = reverse <$> getKey' ""
        where getKey' chars = do
                char <- getChar
                more <- hReady stdin
                (if more then getKey' else return) (char : chars)

-- Main UI / IO
main :: IO ()
main = do
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "                      MATHFUN Haskell Album Rating Data"
        putStrLn "                             Student: UP853829"
        -- putStrLn ("        Successfully loaded " ++ show(length albumFormattedDB) ++ "albums!")
        putStrLn "-------------------------------------------------------------------------------\n"


        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        putStrLn "--------------------------------Menu Options-----------------------------------"
        putStrLn "1 | Demo 1 - Display All Albums In list"
        putStrLn "2 | Demo 2 - Display Top 10"
        putStrLn "3 | Demo 3 - Display Albums Between Two Years"
        putStrLn "4 | Demo 4 - Display All Albumbs Beginning With A Prefix"
        putStrLn "5 | Demo 5 - Display The Sum Of An Artist's Total Sales"
        putStrLn "6 | Demo 6 - Display Artists With Occurences"
        putStrLn "7 | Demo 7 - Update The Current List With A Give Index"
        putStrLn "8 | Demo 8 - Update An Artist's Sales"
        putStrLn "0 | Save and Exit"
        putStrLn "-------------------------------------------------------------------------------\n"
        putStr "Please Enter A Valid Option: "

        key <- getKey

        when (key /= "\ESC") $ do
        case key of
                "1" -> demo 1
                "2" -> demo 2
                "3" -> demo 3
                "4" -> demo 4
                "5" -> demo 5
                "6" -> demo 6
                "7" -> demo 7
                "8" -> demo 8
                -- "0" -> saveDB
                _   -> return ()
        main