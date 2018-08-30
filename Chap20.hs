module Chap20 where

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a


-- Exercises


sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\val acc -> acc || val == x) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr minimumMaybe Nothing

minimumMaybe :: (Ord a) => a -> Maybe a -> Maybe a
minimumMaybe x (Just y) = Just $ min x y
minimumMaybe x _ = Just x

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr maximumMaybe Nothing

maximumMaybe :: (Ord a) => a -> Maybe a -> Maybe a
maximumMaybe x (Just y) = Just $ max x y
maximumMaybe x _ = Just x

null' :: (Foldable t) => t a -> Bool
null' = foldr (\val acc -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (const (+1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\val acc -> mappend (f val) acc) mempty


-- Chapter Exercises

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr f x xs = x

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f init (Two x y) = f y init

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f init (Three x y z) = f z init

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f init (Three' x y z) = f y (f z init)

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f init (Four' x y z t) = f y (f z (f t init))

filterF :: (Applicative f, Foldable f, Monoid (f a)) =>
           (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if (f x) then pure x else mempty)