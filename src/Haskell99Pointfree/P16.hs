{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P16
    ( p16_3
    ) where

import Data.Bool.HT (if')
import Control.Applicative (liftA3, (<*>))
import Control.Lens
import Control.Monad.Extra (ifM)
import Control.Monad (ap, liftM2, join)
import Data.Function
import Debug.Trace

--Problem 16: drop every nth element from a list
--of cource instead of flip one could change the order of parameters
--does not work for infinite lists
p16_1 :: [a] -> Int -> Maybe [a]
p16_1 = flip (liftM2 ( `if'` const Nothing) ( < 1) ( ((Just . reverse . (^._4) . until (null . (^._3)) ( ifM (liftM2 (==) (^._1) (^._2))  ifMTrueBranch ifMFalseBranch    )) . ) .  (,1,,[])))    -- (current position, which elements to drop , original list , resulting list)
  where
    ifMTrueBranch = over _3 tail . set _2  1
    ifMFalseBranch =  over _3 tail . join (set _4 . liftM2 (:) (head . (^._3) ) (^._4) ) . over _2 (+1)

--more convoluted, without unnecessary droping of parameters (const) and without the beginning flip


--using foldl
--does not work for infinite lists
p16_3 :: [a] -> Int -> [a]
p16_3 = (  . ((,1,[]) . join  (  flip if' 1 . (< 1)) )) . (( reverse . (^._3)) . )   . flip (foldl ( ( . ((over _2 (+1) . ) . over _3 . (:) )) . flip (ifM (liftM2 (==) (^._1) (^._2)) (set _2 1))  ))


-- using filter with cycle [1,2,3] and zip, if n is negative or zero  , no element will be dropped
p16_4 :: [a] -> Int -> [a]
p16_4 =  (map fst . ) . ap (filter . ( . snd) . (/=) )  . ( . (cycle . enumFromTo (1::Int) . max 1 )) .  zip

{-
--variation of p16''' returning Nothing on invalid integer input (like zero or negative numbers)
p16_5 :: [a] -> Int -> Maybe [a]
p16_5 = flip (   join  (  (  .     join    (zip . (cycle . enumFromTo (1::Int)  ))  )  .  (flip id) (const Nothing)   . if'  . (>=) 0 ))

--simler version of p16_5
p16_6 :: [a] -> Int -> Maybe [a]
p16_6 =
-}
