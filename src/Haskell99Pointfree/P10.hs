module Haskell99Pointfree.P10
    (
    ) where

import Control.Applicative
import Data.List
import Data.Bool.HT

p10, p10' :: Eq a =>  [a] -> [(a,Integer)]


p10 = map (liftA2 (,) head genericLength) .  group


p10' = undefined -- ifThenElse
