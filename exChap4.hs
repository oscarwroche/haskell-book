module ExChap4 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tuple1 tuple2 = ((snd tuple1, snd tuple2), (fst tuple1, fst tuple2))

x = (+)
f0 xs = x w 1
      where w = length xs

id0 = \x -> x

f1 = \(x : xs) -> x

f2 (a, b) = a
