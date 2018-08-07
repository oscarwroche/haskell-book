module Chap8 where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
three' = (inc . inc . inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times -1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- Exercises

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum n = n + recSum(n - 1)

recMult :: (Integral a) => a -> a -> a
recMult x 0 = 0
recMult x y = x + recMult x (y-1)

mc91 :: Int -> Int
mc91 n = case lessThan100 of
  True -> mc91(mc91(n + 11))
  False -> n - 10
  where lessThan100 = n <= 100
