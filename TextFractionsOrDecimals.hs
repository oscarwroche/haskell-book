{-# LANGUAGE OverloadedStrings #-}

module TextFractionsOrDecimals where

import Control.Applicative
import Text.Trifecta
import TextFractions (parseFraction)

parseD :: Parser Double
parseD = do
  beforeComma <- decimal
  char '.'
  afterComma <- decimal
  return $ (fromIntegral beforeComma) + (convertDec afterComma)

convertDec :: Integer -> Double
convertDec x = (fromIntegral x) / (fromIntegral $ length $ show x)

parseFD :: Parser (Either Rational Double)
parseFD = 
      (Left <$> try parseFraction)
  <|> (Right <$> parseD)

shouldWork = "1/2"
decimalEx = "1.00"
shouldntWork = "1./0/5.6"

main = do
  print $ parseString parseFD mempty shouldWork
  print $ parseString parseFD mempty decimalEx
  print $ parseString parseFD mempty shouldntWork
