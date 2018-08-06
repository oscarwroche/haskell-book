module ExChap3 where

thirdLetter :: [a] -> a
thirdLetter x = head $ drop 2 x

letterIndex :: Int -> Char
letterIndex n = head $ drop n "Salut les pelos"