module Chap9 where

import Data.Bool (bool)
import Data.Char

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- Exercises

eft :: (Eq a, Enum a) => a -> a -> [a]
eft a b
  | a == b = [b]
  | otherwise = a:(eft (succ a) b)

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd _ _ = []

myWords :: String -> [String]
myWords "" = []
myWords ((' '):xs) = myWords xs
myWords s = (takeWhile (/= ' ') s):myWords(dropWhile (/= ' ') s)

myX :: Char -> String -> [String]
myX sep s = go s
  where go "" = []
        go (x:xs)
          | x == sep = go xs
          | otherwise = (takeWhile (/= sep) (x:xs)):(go (dropWhile (/= sep) (x:xs)))

itIsMystery xs = map (\x -> elem x "aeiou") xs

foldBoolApp list = (\x -> bool x (-x) (x==3)) list

myFilter :: String -> [String]
myFilter s = filter (\x -> not $ elem x ["the", "a", "and"]) $ words s

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys)= (x, y):(zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys)= (f x y):(zipWith' f xs ys)

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

-- Data.Char exercise

upperChars :: String -> String
upperChars = filter isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x):(toUpperS xs)

toUpperS  :: String -> String
toUpperS "" = ""
toUpperS (x:xs) = (toUpper x):(toUpperS xs)

cappedHead :: String -> Char
cappedHead = toUpper . head

-- Standard Functions

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = (f x) || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish [x] = x
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f (x:xs) = if f x maxTail == GT then x else maxTail
                       where maxTail = myMaximumBy f xs 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:xs) = if f x minTail == LT then x else minTail
                       where minTail = myMinimumBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare