module Haskell99Pointfree.P10
    (
    ) where

import Control.Applicative
import Data.List
import Data.Bool.HT

p10_1 :: Eq a =>  [a] -> [(a,Integer)]
p10_1 = map (liftA2 (,) head genericLength) .  group

{-
--takewhile and dropWhile
p10_2 :: Eq a =>  [a] -> [(a,Integer)]
p10_2 =
-}
