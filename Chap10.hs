module Chap10 where

exA = foldr (++) [] ["woot", "WOOT", "woot"]
exB = foldr max ' ' "fear is the little death"
exC = foldr (&&) True [False, True]
exD = foldr (||) True [False, True]
exE = foldl (flip ((++) . show)) "" [1..5]
exF = foldr (flip const) 'a' [1..5]
exG = foldr (flip const) 0 "tacos"
exH = foldl const 0 "burritos"
exI = foldl const 'z' [1..5]

stops = "pbtdkg"
vowels = "aeiou"

letterCombs :: [(Char, Char, Char)]
letterCombs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

letterCombs' :: [(Char, Char, Char)]
letterCombs' = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

seekritFunc x = fromIntegral(sum (map length (words x))) / fromIntegral(length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\val acc -> acc || (f val)) False

myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr (\val acc -> (val == elem) || acc) False

myReverse :: [a] -> [a]
myReverse = foldr (\val acc -> acc ++ [val]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\val acc -> f(val):acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\val acc -> if f(val) then val:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\val acc -> val ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\val acc -> f(val) ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list = foldl (\acc val -> if f acc val == GT then acc else val) (head list) list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list = foldl (\acc val -> if f acc val == LT then acc else val) (head list) list
