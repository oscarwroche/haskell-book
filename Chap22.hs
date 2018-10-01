{-# LANGUAGE InstanceSigs #-}

module Chap22 where

import Control.Applicative
import Control.Comonad
import Data.Char
import Text.Show.Functions

hurr :: Num a => a -> a
hurr = (* 2)

durr :: Num a => a -> a
durr = (+ 10)

m :: (Num a) => a -> a
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled1 :: [Char] -> ([Char], [Char])
tupled1 = do
    a <- rev
    b <- cap
    return (a, b)

tupled2 :: [Char] -> ([Char], [Char])
tupled2 = cap >>= \a -> rev >>= \b -> return (a, b)

newtype Reader r a = Reader { runReader :: r -> a } deriving (Show)

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (>>=) (Reader ra) aRb =
    Reader $ \r -> (runReader (aRb (ra r))) r



ask :: Reader a a
ask = Reader $ id

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Oscar")
         (DogName "Medor")
         (Address "Francois Giraud")

victor :: Person
victor = Person (HumanName "Victor")
                (DogName "Jack")
                (Address "London")

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 l f g = l <$> f <*> g

asks :: (r -> a) -> Reader r a
asks f = Reader f

getDogR :: Reader Person Dog
getDogR = Dog <$> (Reader dogName) <*> (Reader address)

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor t, Foldable t, Num a) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Reader Person Dog
getDogRM = do
  n <- Reader $ \p -> dogName p
  a <- Reader $ \p -> address p
  return $ Dog n a

