{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P09
    (
    ) where


import Data.List
import Control.Lens
import Data.Bool.HT (if')
import Control.Applicative (liftA2 , liftA3)
import Control.Monad ((=<<), join , ap)
import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)
import Data.Function ((&))

--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group

--using foldr
p09_2 :: Eq a => [a] -> [[a]]
p09_2 = ifM null (const []) ( (((^._3) . ) . foldr foldrHelper)  =<<   join ((,,[]). head)    )
  where
    foldrHelper :: Eq a => a -> (a , [a], [[a]]) -> (a, [a], [[a]])
    foldrHelper = ap (flip ifM ifMTrueBranch .  ( . (^._1))  .  (==))   (( . (liftA2 (:) (^._2) (^._3))) . (,[],))
      where
        ifMTrueBranch = join (over _2 . (:) . (^._1))
--using until with takewhile and dropwhile

{-
--using fix
p09_2 :: Eq a => [a] -> [[a]];
p09_2 = fix (   ( . (flip if' []  . null) ) . if'  .   )
-}
--using fix tailrecursively
