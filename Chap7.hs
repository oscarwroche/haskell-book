{-# LANGUAGE NoMonomorphismRestriction #-}

module Chap7 where

myNum :: Integer
myNum = 1

myVal f = myNum

addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x = let y = 5 in
              "the integer was " ++ show x
              ++ "and y was: " ++ show y

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

data WherePenguinsLive =
    Galapagos
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antartica
macaroni = Peng Antartica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarticPenguin :: Penguin -> Bool
antarticPenguin (Peng Antartica) = True
antarticPenguin _ = False

antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p = (galapagosPenguin p) || (antarticPenguin p)

funcZ x =
  case x + 1 == 1 of
    True -> "yeah"
    False -> "no"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "ey whass gud"
    False -> putStrLn "shut up bish"
  where cool = coolness == "frosty"

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> (flip reportBoss) e e'

myAbs :: Integer -> Integer
myAbs x
   | x < 0 = (-x)
   | otherwise = x

bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "right on"
    | otherwise = "not right"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

-- Exercises

mTh x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC x y =
  case greater of
    True -> x
    _ -> y
  where greater = x > y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    _ -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where d = (fst $ divMod (snd $ divMod x 100) 10)

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b == True = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f(a), c)
