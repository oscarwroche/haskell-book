module Jammin where

import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam { fruit :: Fruit
      , quantity :: Int}
      deriving (Eq, Show, Ord)

row1 = Jam Peach 6
row2 = Jam Apple 10
row3 = Jam Apple 2
row4 = Jam Plum 7
row5 = Jam Blackberry 1
row6 = Jam Peach 7
allJam = [row1, row2, row3, row4, row5, row6]

numJars :: [JamJars] -> Int
numJars = foldr ((+) . quantity) 0

maxJars :: [JamJars] -> JamJars
maxJars jars = foldr (\val acc -> if val > acc then val else acc) (head jars) jars

sortByFruit :: [JamJars] -> [JamJars]
sortByFruit = sortBy compareKind
              where compareKind (Jam k _) (Jam k' _) = compare k k'

groupByFruit :: [JamJars] -> [[JamJars]]
groupByFruit = (groupBy (\a b -> (fruit a) == (fruit b))) . sortByFruit
