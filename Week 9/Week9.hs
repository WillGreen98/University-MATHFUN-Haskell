helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else do
        putStrLn (isPalindrome str)
        palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9

celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

-- worksheet
greeting :: IO ()
greeting = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("Hello, " ++ name)

addTwoNumbers :: IO ()
addTwoNumbers = do
  putStr "Enter a number: "
  n1 <- getLine
  putStr "Enter the second number: "
  n2 <- getLine
  let sum = (read n1 :: Int) + (read n2 :: Int)
  putStrLn ("The result is: " ++ show sum)

copyFile :: IO ()
copyFile = do
  putStr "Enter the filename you want to copy: "
  name <- getLine
  contents <- readFile name
  putStr "Enter the name of the new file: "
  copyName <- getLine
  writeFile copyName contents

buildList :: [String] -> IO ()
buildList array = do
  putStr "Enter a line: "
  str <- getLine
  if str == "" then
    return ()
    else do
      let newArray = array ++ [str]
      putStrLn ("List is now " ++ show newArray)
      buildList newArray

listBuilder :: IO ()
listBuilder = do
  buildList []

addUp :: [Int] -> Int
addUp [] = 0
addUp (x:xs) = foldr (+) 0 (x:xs)

buildAddUpList :: [Int] -> IO ()
buildAddUpList array = do
  putStr "Enter a number: "
  n <- getLine
  if n == "" then
    putStrLn ("The sum of the numbers is: " ++ show (addUp array))
    else do
      let newArray = array ++ [(read n :: Int)]
      putStrLn ("List is now " ++ show newArray)
      buildAddUpList newArray

addUpBuilder :: IO ()
addUpBuilder = do
  buildAddUpList []
