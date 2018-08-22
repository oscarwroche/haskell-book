module Chap15Ex where

import Data.Semigroup
import Test.QuickCheck (arbitrary,
                        coarbitrary,
                        Arbitrary,
                        CoArbitrary,
                        elements,
                        Gen,
                        quickCheck,
                        verboseCheck,
                        sample,
                        frequency,
                        variant)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String 
                   -> Identity String
                   -> Identity String
                   -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String String
              -> Two String String
              -> Two String String
              -> Bool

newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- elements [True, False]
    return $ BoolConj a

type BoolConjAssoc = BoolConj
                   -> BoolConj
                   -> BoolConj
                   -> Bool

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where 
  (Snd x) <> _ = Snd x
  _ <> (Snd x) = Snd x
  (Fst x) <> (Fst y) = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Fst a))
              , (1, return (Snd b)) ]

type OrAssoc = Or String Int
             -> Or String Int
             -> Or String Int
             -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance CoArbitrary (Combine a b) where
  coarbitrary (Combine f) = variant 0

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

newtype Comp a =
  Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (g . f)

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Failure x) <> (Success _) = Failure x
  (Success _) <> (Failure y) = Failure y
  (Success x) <> (Success _) = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Failure a))
              , (1, return (Success b)) ]

type ValidationAssoc = Validation String String
                     -> Validation String String
                     -> Validation String String
                     -> Bool

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight(Success x) <> AccumulateRight(Success y)  = AccumulateRight(Success(x <> y))
  AccumulateRight(Success x) <> AccumulateRight(Failure y)  = AccumulateRight(Success x)
  AccumulateRight(Failure x) <> AccumulateRight(Success y)  = AccumulateRight(Success y)
  AccumulateRight(Failure x) <> AccumulateRight(Failure y)  = AccumulateRight(Failure y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (AccumulateRight a))
              , (1, return (AccumulateRight b)) ]

type AccumulateRightAssoc = AccumulateRight Int String
                          -> AccumulateRight Int String
                          -> AccumulateRight Int String
                          -> Bool

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth(Success x) <> AccumulateBoth(Success y)  = AccumulateBoth(Success(x <> y))
  AccumulateBoth(Success x) <> AccumulateBoth(Failure y)  = AccumulateBoth(Success x)
  AccumulateBoth(Failure x) <> AccumulateBoth(Success y)  = AccumulateBoth(Success y)
  AccumulateBoth(Failure x) <> AccumulateBoth(Failure y)  = AccumulateBoth(Failure (x <> y))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (AccumulateBoth a))
              , (1, return (AccumulateBoth b)) ]

type AccumulateBothAssoc = AccumulateBoth String String
                          -> AccumulateBoth String String
                          -> AccumulateBoth String String
                          -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  verboseCheck (semigroupAssoc :: AccumulateBothAssoc)