import Data.Char (isDigit)
import System.IO

charToInt :: Char -> Int
charToInt c = fromEnum c - fromEnum '0'

readFileLines :: FilePath -> IO [String]
readFileLines path = do
  handle <- openFile path ReadMode
  lines <- readLines handle
  hClose handle
  return lines

readLines :: Handle -> IO [String]
readLines handle = do
  eof <- hIsEOF handle
  if eof
    then return []
    else do
      line <- hGetLine handle
      rest <- readLines handle
      return (line : rest)

findFirstLastDigit :: String -> Int
findFirstLastDigit (x : xs)
  | isDigit x = charToInt x
  | otherwise = findFirstLastDigit xs

sumLinesDigits :: [String] -> Int
sumLinesDigits lines = map findFirstLastDigit
main :: IO ()
  do
      lines <- readFileLines "codes.txt"
    mapM_
    putStrLn
    lines
