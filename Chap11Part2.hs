module Chap11Part2 where

import Data.Char
import Data.Bool
import Data.Maybe
import Data.String
import Data.List

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
  Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name :: Name 
            , acre :: Acres
            , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _           -> False


data Automobile = Null
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                deriving (Eq, Show)

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- Higher-kinded Datatypes

data Silly a b c d = MkSilly a b c d deriving Show

data Product a b =
  a :&: b
  deriving (Eq, Show)

data List a = Nil | Cons a (List a) deriving Show

-- Binary Trees

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : ((preorder left) ++ (preorder right))

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc left) right)

-- Chapter Exercises

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

vigenereCipher :: String -> String -> String
vigenereCipher ys "" = ""
vigenereCipher ys (' ':xs) = ' ':vigenereCipher ys xs 
vigenereCipher (y:ys) (x:xs) = (f x):(vigenereCipher (ys ++ [y]) xs)
                        where f = chr . g . (+ (n `mod` 26)) . ord
                              g = (\x -> bool x (((+96) . (`mod` 122)) x) (x > 122))
                              n = ord y - 96

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] ys = True
isSubsequence (x:xs) ys = if elem x ys then isSubsequence xs ys else False                      

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f (words s)
                    where f w@(x:xs) = (w, (toUpper x):xs)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (' ':xs) = ' ':capitalizeParagraph xs
capitalizeParagraph (x:xs) = toUpper x:go xs
                           where go ('.':xs) = '.':capitalizeParagraph xs
                                 go (x:xs) = x:go xs

-- Phone

type Digit = Char

data Button = Button Digit String

data Phone = Phone [Button]

daPhone :: Phone
daPhone = Phone
  [ Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '*' ""
  , Button '0' " 0"
  , Button '#' "."
  ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]
  -- validButtons = "1234567890*#"
-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone phone) char = (ifCapital char) ++ (map g $ filter f phone)
                         where f (Button x y) = elem (toLower char) y
                               g (Button x y) = (x, (+1) $ fromJust $ findIndex (==toLower char) y)
                               ifCapital char = if isUpper char then [('*', 1)] else []

-- assuming the default phone definition
-- 'a' -> ('2', 1)
  -- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead x s = foldr (++) [] (map (reverseTaps x) s)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

-- Hutton's razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (+) (eval expr1) (eval expr2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)

