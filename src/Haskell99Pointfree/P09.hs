{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P09
    (
    ) where


import Data.List
import Control.Lens
import Data.Bool.HT
import Control.Applicative
import Control.Monad

--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group

--using fix
p09_2 :: Eq a => [a] -> [[a]];
p09_2 = fix (   ( . (flip if' []  . null) ) .  (.))

--using fix tailrecursively
