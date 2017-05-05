{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P35
    (
    ) where

import Control.Lens
import Control.Applicative (liftA2)
import Control.Monad.Extra (ifM)
import Control.Monad (join)

--trivial solution
p35_1 :: Int -> [Int]
p35_1 = reverse .view _3 . until (liftA2 (>) (view _1) (view _2)) nextStep  . (2,,[])  . abs
  where
    nextStep :: (Int,Int,[Int]) -> (Int,Int,[Int])
    nextStep = ifM ( (==0) . liftA2 mod (view _2) (view _1)) (   join ( liftA2 (.) (over _2 . flip div) (over _3 . (:)) . view _1 )     ) (over _1 (+1))
