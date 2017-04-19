{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P09
    (
    ) where


import Data.List
import Control.Lens
import Data.Bool.HT
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group



{-
p09_2 :: Eq a => [a] -> [[a]]
p02_2 lss  =  if null lss then [] else  fix (\recu curr currLs res ls -> )
-}
{-
--using fix
p09_2 :: Eq a => [a] -> [[a]];
p09_2 = fix (   ( . (flip if' []  . null) ) . if'  .   )
-}
--using fix tailrecursively
