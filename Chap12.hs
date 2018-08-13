module Chap12 where

import Data.String
import Data.List

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving Eq

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeTooLow = "AgeTooLow"
  | otherwise = "???"

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >=0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


data Example a = Blah | RoofGoats | Woot a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe s = unwords . go . words $ s
               where go [] = []
                     go (x:xs) = case notThe x of
                       Nothing -> "a":go xs
                       Just x -> x:go xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s)
               where go [] = 0
                     go [x] = 0
                     go (x0:x1:xs) = case notThe x0 of
                       Nothing -> if elem (head x1) "aeiouy" then 1 + go xs else go xs
                       Just x0 -> go (x1:xs)

isVowel :: Char -> Bool
isVowel a = elem a "aeiouy"

countVowels :: String -> Int
countVowels = length . (filter isVowel)

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiouy"

mkWord :: String -> Maybe Word'
mkWord s = case (length s) - (countVowels s) > (countVowels s) of
  True -> Just(Word' s)
  False -> Nothing

-- It's only natural

data Nat = 
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n = case n >= 0 of 
                 True -> Just (go(n))
                 False -> Nothing
                 where go 0 = Zero
                       go k = Succ (go (k - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x:catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case length xs == length ys of
               True -> Just (ys)
               False -> Nothing
               where ys = catMaybes xs

lefts' :: [Either a b] -> [a]
lefts' = foldr ((++) . f) []
         where f (Left a) = [a]
               f _ = []

rights' :: [Either a b] -> [b]
rights' = foldr ((++) . f) []
         where f (Right a) = [a]
               f _ = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just $ f x)

-- Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f init = init:go f init
  where go f x = f(x):go f (f x)

myUnfoldr :: (b -> Maybe(a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just t -> fst t : (myUnfoldr f $ snd t)
  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Just t -> Node (unfold f (fst3 t)) (snd3 t) (unfold f (thd3 t))
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
              where f x = case x < n of
                            True -> Just(x + 1, x, x + 1)
                            False -> Nothing
