{-# LANGUAGE TypeSynonymInstances #-}
module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type ZL = ZipList
type SM = Sum

instance Monoid a => Monoid (ZL a) where
  mempty = ZL []
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZL a) where
  arbitrary = ZL <$> arbitrary

instance  Arbitrary a => Arbitrary (SM a) where
  arbitrary = SM <$> arbitrary

instance Eq a => EqProp (ZL a) where (=-=) = eq 