module Chap18 where

import Control.Monad
import Control.Applicative ((*>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join (fmap f x)

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "something else"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "something else"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name please :"
  name <- getLine
  putStrLn "age please :"
  age <- getLine
  putStrLn ("why hello: "
           ++ name ++ " who is: "
           ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name please:" >>
  getLine >>=
  \name ->
  putStrLn "age please:" >>
  getLine >>=
  \age ->
  putStrLn ("why hello: "
           ++ name ++ " who is: "
           ++ age ++ " years old.")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
  } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name age weight = do
  name <- noEmpty name
  age <- noNegative age
  weight <- noNegative weight
  weightCheck (Cow name age weight)

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
  a <- f
  b <- g
  c <- h
  return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething' = do
  a <- f
  b <- g
  c <- h
  zed' a b c

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears years coders
    else Right $ Shop founded programmers

 
-- Either Monad Exercise

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second x) = Second (f x) 

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second x) = Second $ f x
  (<*>) (First x) _ = First x
  (<*>) (Second f) (First x) = First x

instance Monad (Sum a) where
  return = pure
  (>>=) (Second x) f = f x
  (>>=) (First x) _ = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements[First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- Application and Composition


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hi how old are you"


-- Chapter Exercises


data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg g = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
   return = pure
   (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where (=-=) = eq

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

instance Applicative List where
  pure x = Cons x Nil
  (<*>) fs vs = fold (\f acc -> append (fmap f vs) acc) Nil fs

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = append (f x) (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

instance Eq a => EqProp (List a) where (=-=) = eq

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = x >>= (\ x' -> y >>=(\ y' -> return $ f x' y'))

a :: Monad m => m a -> m (a -> b) -> m b
a x f = x >>= (\x' -> f >>= (\f' -> return $ f' x'))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = (f x) >>= (\x' -> fmap (x':) (meh xs f))

flipType :: Monad m => [m a] -> m [a]
flipType ms = meh ms id

main = do
  quickBatch $ monad (undefined :: Nope (Int, String, Int))
  quickBatch $ monad (undefined :: Sum (Int, String, Int) (Int, String, Int))
  quickBatch $ monad (undefined :: Identity (Int, String, Int))
  quickBatch $ monad (undefined :: List (Int, String, Int))