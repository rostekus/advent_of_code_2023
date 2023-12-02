import Data.Char (digitToInt, isDigit)
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

getLastDigit :: String -> Int
getLastDigit str = digitToInt $ last (filter (`elem` ['0' .. '9']) str)

getFirstDigit :: String -> Int
getFirstDigit str = digitToInt $ head (filter (`elem` ['0' .. '9']) str)

sumLinesDigits :: [String] -> Int
sumLinesDigits lines =
  let sumFirst = 10 * sum (map getFirstDigit lines)
      sumLast = sum $ map getLastDigit lines
   in sumFirst + sumLast

main :: IO ()
main = do
  lines <- readFileLines "codes.txt"
  let sumDigits = sumLinesDigits lines
  print sumDigits
