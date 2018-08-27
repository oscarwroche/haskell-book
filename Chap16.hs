{-# LANGUAGE RankNTypes #-}

module Chap16 where

import Test.QuickCheck
import Test.QuickCheck.Function

data FixMePls a =
    FixMe a
  | Pls
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap f (FixMe a) = FixMe (f a)
  fmap _ Pls = Pls

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap _  WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

data CountingGood a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg (n) (f a)

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = fmap liftedReplace

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f(f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (\x -> read x::Integer) (fmap ("123"++) $ fmap show ioi)
    in fmap (*3) changed

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) =>
                      f a
                   -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercises

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IntToInt = Fun Int Int

type IdentityID = Identity Int -> Bool
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

type PairID = Pair Int -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoID = Two String Int -> Bool
type TwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeID = Three String Double Int -> Bool
type ThreeFC = Three String Double Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z ) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

type Three'ID = Three' String Int -> Bool
type Three'FC = Three' String Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck(functorIdentity :: IdentityID)
  quickCheck(functorCompose' :: IdentityFC)
  quickCheck(functorIdentity :: PairID)
  quickCheck(functorCompose' :: PairFC)
  quickCheck(functorIdentity :: TwoID)
  quickCheck(functorCompose' :: TwoFC)
  quickCheck(functorIdentity :: ThreeID)
  quickCheck(functorCompose' :: ThreeFC)
  quickCheck(functorIdentity :: Three'ID)
  quickCheck(functorCompose' :: ThreeFC)

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a =
    Nope
  | Yep a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f Nope = Nope
  fmap f (Yep a) = Yep $ f a

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)


getInt :: IO Int
getInt = fmap read getLine

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
