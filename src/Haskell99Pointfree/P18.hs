{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P18
    (
    ) where

import Control.Monad (ap, join, liftM2 , liftM3)
import Control.Monad.Extra (ifM)
import Control.Applicative ((<*>))
import Control.Lens
import Data.Bool.HT

p18_1 :: [a] -> Int -> Int -> [a]
p18_1 =  (  . (subtract 1  . max 1)) . ap  ( (. flip take) . flip (  .  ) . subtract )  . flip drop

--using foldl
p18_2 :: [a] -> Int -> Int -> [a]
p18_2 =  (. (,,1,[]) ) . (.)  . ((reverse . view _4) .) . flip (foldl ( (over _3 (+1) . )  . liftM3 if' ( join ( ( . view _3) . flip . liftM2 (&&) . (<=) . view _1)  <*>  ( (>=) . view _2) )  ( ( . (:) ) . flip (over _4 )) const     )    )


p18_3 :: [a] -> Int -> Int -> [a]
p18_3 = undefined
