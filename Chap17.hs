module Chap17 where

import Control.Applicative
import Data.List (elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> a <*> b

xs = [1, 2, 3]
ys = [4, 5, 6]

s :: Maybe Integer
s = lookup 3 $ zip xs ys

t :: Maybe Integer
t = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) s t

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)


validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = liftA2 Person (mkName n) (mkAddress a)

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name
      <*> noNegative age
      <*> noNegative weight

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight =
  liftA3 Cow (noEmpty name)
             (noNegative age)
             (noNegative weight)

-- Exercise

exA = const <$> Just "Hello" <*> pure "World"
exB = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold ::(a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t)= f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as 

instance Applicative List where
  pure x = Cons x Nil
  (<*>) fs vs = fold (\f acc -> append (fmap f vs) acc) Nil fs

take' :: Int -> List a -> List a
take' = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' (zipWith' id xs ys)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error e
  | Success' a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (Second f) (Second x) = Second $ f x
  (<*>) (Second _) (First x) = First x
  (<*>) (First x) (Second _) = First x
  (<*>) (First x) (First _) = First x

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success' x) = Success' $ f x

instance (Monoid e) => Applicative (Validation e) where
  pure x = Success' x
  (<*>) (Success' f) (Success' s) = Success' $ f s
  (<*>) (Success' _) (Error x) = Error x
  (<*>) (Error x) (Success' _) = Error x
  (<*>) (Error x) (Error y) = Error (mappend x y)

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements[First a, Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements[Success' a, Error b]

type SumType = Sum String (Int, Int, Int)
type ValidationType = Validation String (Int, Int, Int)

main :: IO ()
main = do
  quickBatch (applicative $ ((Second (4 :: Int, 5 :: Int, 6 :: Int)) :: SumType))
  verboseBatch (applicative $ ((Success' (4 :: Int, 5 :: Int, 6 :: Int)) :: ValidationType))
