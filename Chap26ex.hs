module Chap26ex where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader $ \x -> x + 1