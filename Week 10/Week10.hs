-- worksheet

-- Functional Code
addWord :: String -> [String] -> [String]
addWord str array = array ++ [str]

wordsToString :: [String] -> String
wordsToString [] = ""
wordsToString [x] = x
wordsToString (x:xs) = x ++ "\n" ++ wordsToString xs

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength _ [] = []
wordsOfLength n (x:xs) = [word | word <- (x:xs), length word == n]

-- User Interface Code

-- 1st exercise

-- main :: IO ()
-- main = do
--   let file = "words.txt"
--   listOfStrings <- getListOfStrings file
--   let word = "lemon"
--   let newList = addWord word listOfStrings
--   let listAsString = wordsToString newList
--   putStrLn ("The words in the file are:\n" ++ listAsString)
--   writeFile file (show listAsString)

getListOfStrings :: IO [String]
getListOfStrings = do
  let file = "words.txt"
  contents <- readFile file
  return (read contents :: [String])

-- 2nd exercise
main1 :: IO ()
main1 = do
  listOfStrings <- getListOfStrings
  option <- startMenu
  executeOption option listOfStrings

startMenu :: IO String
startMenu = do
  putStrLn ""
  putStrLn "Enter the number of the option desired: "
  putStrLn "1. Add a word to the list"
  putStrLn "2. Display all words"
  putStrLn "3. Display all words of a given length"
  putStrLn "4. Exit"
  option <- getLine
  return option

executeOption :: String -> [String] -> IO ()
executeOption "1" listOfStrings = do
  putStrLn "Enter the word: "
  word <- getLine
  let newList = addWord word listOfStrings
  option <- startMenu
  executeOption option newList
executeOption "2" listOfStrings = do
  putStrLn (wordsToString listOfStrings)
  option <- startMenu
  executeOption option listOfStrings
executeOption "3" listOfStrings = do
  putStrLn "Choose a length: "
  lenStr <- getLine
  let len = read lenStr :: Int
  let filteredWords = wordsOfLength len listOfStrings
  putStrLn (wordsToString filteredWords)
  option <- startMenu
  executeOption option listOfStrings
executeOption "4" listOfStrings = do
  writeFile "words.txt" (wordsToString listOfStrings)
  return ()
executeOption _ listOfStrings = do
  putStrLn "Choose a valid option"
  option <- startMenu
  executeOption option listOfStrings
