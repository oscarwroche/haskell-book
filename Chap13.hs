module Chap13 where

import System.Exit (exitSuccess)
import Data.Char
import Data.Bool
import Control.Monad

vigenereCipherUser :: IO ()
vigenereCipherUser = do
  putStrLn "Pick a word"
  word <- getLine
  putStrLn "Pick a cipher"
  cipher <- getLine
  putStrLn $ "The encoded word is :" ++ vigenereCipher cipher word
  return ()

vigenereCipher :: String -> String -> String
vigenereCipher ys "" = ""
vigenereCipher ys (' ':xs) = ' ':vigenereCipher ys xs 
vigenereCipher (y:ys) (x:xs) = (f x):(vigenereCipher (ys ++ [y]) xs)
                        where f = chr . g . (+ (n `mod` 26)) . ord
                              g = (\x -> bool x (((+96) . (`mod` 122)) x) (x > 122))
                              n = ord y - 96

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  let strippedLine1 = map toLower $ filter isAlphanumeric line1
  case (strippedLine1 == reverse strippedLine1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

isAlphanumeric :: Char -> Bool
isAlphanumeric c = elem c ['a'..'z'] || elem c ['A'..'Z']

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Gimme a name !"
  name <- getLine
  putStrLn "Gimme an age !"
  age <- getLine
  let person = mkPerson name (read age::Integer)
  case person of 
    Right p -> putStrLn $ "Yay - it's a person " ++ show p
    Left p -> putStrLn $ show p
