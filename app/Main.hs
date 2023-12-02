module Main where

import Data.Char (isDigit)

findFirstDigit :: String -> Char
findFirstDigit (x : xs)
  | isDigit x = x -- If the current character is a digit, return it
  | otherwise = findFirstDigit xs -- Move to the next character

-- Run the tests
main :: IO ()
main = do
  let word = "1hello"
  print $ findFirstDigit word
