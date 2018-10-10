{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty

import Data.Monoid (mconcat)
import Control.Monad.Trans.Class

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (lift :: IO a -> ActionM a) $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]