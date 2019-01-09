module Chap27 where

possiblyKaboom =
  \f -> f fst snd (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

possiblyKaboom' b =
  case b of
    True -> fst tup
    False -> snd tup
  where tup = (0, undefined)