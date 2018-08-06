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