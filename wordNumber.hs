module WordNumber where

import Data.List (intersperse)
import Data.Char

digitToWord :: Int -> String
digitToWord n = head $ drop n digits where
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n = digitsFromString(show n)
           where digitsFromString "" = []
                 digitsFromString (x:xs) = [xInt] ++ (digitsFromString xs)
                                           where xInt = digitToInt x

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits