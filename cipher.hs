module Cipher where

import Data.Char
import Data.Bool (bool)

caesarCipher :: Int -> String -> String
caesarCipher n "" = ""
caesarCipher n (x:xs) = (f x):(caesarCipher n xs)
                        where f = chr . g . (+ (n `mod` 26)) . ord
                              g = (\x -> bool x (((+97) . (`mod` 122)) x) (x > 122))
