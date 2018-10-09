module LearnParsers where

import Text.Trifecta
import Control.Monad.Trans.State
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
one'' = one >> eof

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
oneTwo'' = oneTwo >> eof

oneTwoThree :: Parser String
oneTwoThree = choice [string "123", string "12", string "1"]

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p =
  print $ parseString p mempty "123"

testParseString :: Parser String -> String -> IO ()
testParseString p s =
  print $ parseString p mempty s

p' :: Parser [Integer]
p' = some $ do
  i <- token digit
  return (read [i])
