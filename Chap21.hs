{-# LANGUAGE UndecidableInstances #-}

module Chap21 where

data Either' a b =
    Left' a
  | Right' b

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' $ f y

instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _) = z
  foldr f z (Right' y)= f y z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> r = fmap f r

instance Foldable Identity where
  foldr f init (Identity x) = f x init

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (Constant x) <*> (Constant y) = Constant (mappend x y)

instance Foldable (Constant a) where
  foldr f init (Constant x) = init

instance Traversable (Constant a) where
  traverse f (Constant x) = pure (Constant x)

data Optional a =
    Nada
  | Yep a

instance (Foldable Optional, Functor Optional) => Traversable Optional where
  traverse f Nada = pure $ Nada
  traverse f (Yep x) = Yep <$> f x 

data List a =
    Nil
  | Cons a (List a)

instance (Foldable List, Functor List) => Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a x) = Cons <$> f a <*> traverse f x

data Three a b c =
  Three a b c

instance (Foldable (Three a b), Functor (Three a b)) => Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> (f z)

data Three' a b =
  Three' a b b

instance (Foldable (Three' a), Functor (Three' a)) => Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> (f y) <*> (f z)

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor (S n), Foldable(S n), Traversable n) => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node x y z) = mappend (mappend (f y) (foldMap f x)) (foldMap f z)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node x y z) = Node <$> (traverse f x) <*> (f y) <*> (traverse f z)
