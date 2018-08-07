module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = head $ drop n digits where
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = [firstDigit] ++ digits(otherDigits)
    where firstDigit = read (take 1 digitsToString) :: Int
          otherDigits = read (drop 1 digitsToString) :: Int
          digitsToString = show n :: String

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits