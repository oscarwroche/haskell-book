module Chap17Ex where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where (=-=) = eq

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two e f) (Two e' x) = Two (mappend e e') (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' x) = Three (mappend a a')
                                              (mappend b b')
                                              (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' e f g) (Three' e' x y) = Three' (mappend e e')
                                                (f x)
                                                (g y)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three' a b c

main :: IO ()
main = do
  quickBatch(applicative $ Identity ("a", "b", 5 :: Int ))
  quickBatch(applicative $ Pair ("a", "b", 5 :: Int ) ("r", "z", 5 :: Int ))
  quickBatch(applicative $ Two ([4 :: Int], [5 :: Int] , "a") ("a", 'b', [4 :: Int]))
  quickBatch(applicative $ Three ([4 :: Int], [5 :: Int] , "a") ("a", ['b'], [4 :: Int]) ("c", 'b', [4 :: Int]))
  quickBatch(applicative $ Three' ([4 :: Int], [5 :: Int] , "a") ("a", ['b'], [4 :: Int]) ("c", ['y'], [4 :: Int]))

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
