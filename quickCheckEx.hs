module QuickCheckEx where

import Test.QuickCheck
import Data.List (sort)

half x = x / 2

halfIdentity = (*2) . half

genInt :: Gen Int
genInt = elements [1..99]

propHalf :: Double -> Bool
propHalf x = halfIdentity x == x

listOrdered :: [Int] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) (sort xs)
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Property
plusAssociative =
  forAll genInt
  (\x y z -> x + (y + z) == (x + y) + z)

plusCommutative :: Property
plusCommutative =
  forAll genInt
  (\x y -> x + y == y + x)

timesAssociative :: Property
timesAssociative =
  forAll genInt
  (\x y z -> x + (y + z) == (x + y) + z)

timesCommutative :: Property
timesCommutative =
  forAll genInt
  (\x y -> x + y == y + x)

propQuotRem :: Property
propQuotRem =
  forAll (genInt, genInt)
  (\x y -> (quot x y)*y + (rem x y) == x)

main :: IO ()
main = do 
  quickCheck listOrdered
  quickCheck propHalf
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck timesAssociative
  quickCheck timesCommutative
  quickCheck propQuotRem
