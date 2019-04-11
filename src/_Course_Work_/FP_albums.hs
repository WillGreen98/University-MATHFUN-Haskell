-- MATHFUN
-- Functional Programming" Haskell Assignment Program - Albums
-- UP853829

import System.IO
import Data.List
import Control.Monad
import qualified Data.Text as T

-- Menu Imports
import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering), hReady)
import Control.Monad (when)

-- Types -- Define Album type here
let testDataRead = map(read :: String -> String -> Int -> Int -> String) [albums.txt]

data Album = Album {album :: String
                ,   artist :: String
                ,   year :: Int
                ,   noSales :: Int
}

--  Your functional code goes here
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

testData :: [Album]
testData = [albumsReadFile]

albumsToString :: [Album] -> String
newEntry :: String -> String -> Int -> Int -> [Album] -> [Album]

-- Demo function to test basic functionality (without persistence - i.e.)
demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
--demo 2  = putStrLn (albumsToString (top10 testData))
--demo 3  = putStrLn (all albums released between 2000 and 2008 inclusive)
--demo 4  = putStrLn (all albums with titles beginning with "Th")
--demo 5  = putStrLn (total sales figure for "Queen")
--demo 6  = putStrLn (all artists with the number of times they appear in top 50)
--demo 7  = putStrLn (albums after removing 50th album and adding "Progress" by "Take That" from 2010 with 2700000 sales)
--demo 8  = putStrLn (albums after increasing sales of "21" by "Adele" by 400000)

-- Your user interface (and loading/saving) code goes here
menu :: IO ()
menu = do
    putStrLn . unlines $ map concatNums choices
    choice <- getLine
    case validate choice of
        Just n  -> execute . read $ choice
        Nothing -> putStrLn "Please try again"

    menu
    where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
    where isValid []            = Nothing
        isValid ((n, _):_)
                | outOfBounds n = Nothing
                | otherwise     = Just n
        outOfBounds n = (n < 1) || (n > length choices)

        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        key <- getKey
        when (key /= "\ESC") $ do
          case key of
            "w"     -> putStrLn "↑"
            "s"     -> putStrLn "↓"
            "d"     -> putStrLn "→"
            "a"     -> putStrLn "←"
            "\n"    -> putStrLn "⎆"
            "\DEL"  -> putStrLn "⎋"
            _       -> return ()

choices :: [(Int, (String, IO ()))]
choices = zip [1..] [
    ("Demo 1")
]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
    where doExec ((_, (_,f)):_) = f

Menu