{-# LANGUAGE FlexibleInstances #-}

module Chap16Ex where

data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a
  deriving Show

instance Functor (K a) where
  fmap f (K a) = K a 

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K b) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b =
  GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

data LiftItOut f a =
  LiftItOut (f a)
  deriving Show

instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap m (DaWrappa x y) = DaWrappa (fmap m x) (fmap m y)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap m (IgnoringSomething x y) = IgnoringSomething x (fmap m y)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap m (Notorious x y z) = Notorious x y (fmap m z)

data List a =
    Nil
  | Cons a (List a)
  deriving Show

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read(f . g)
