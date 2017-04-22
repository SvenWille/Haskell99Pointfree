{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P10
    ( p10_1,  p10_2
    ) where

import Control.Applicative
import Control.Monad
import Data.List (genericLength, group)
import Haskell99Pointfree.P09
import Control.Arrow

p10_1 :: Eq a =>  [a] -> [(Integer,a)]
p10_1 = map (liftA2 (,) genericLength head) .  p09_1


p10_2 :: Eq a => [a] -> [(Integer,a)]
p10_2 =  join (zipWith ( (. head)  .(,) . genericLength ) ). p09_2

--using arrows
p10_3 :: Eq a => [a] -> [(Int,a)]
p10_3 = map ( length &&& head ) . p09_3
--solutions ignoring P09

--using until, takewhile and dropWhile
p10_4 :: Eq a =>  [a] -> [(Integer,a)]
p10_4 = snd . until (null . fst) nextStep . (,[]) .reverse
  where
    nextStep :: Eq a => ([a],[(Integer,a)]) -> ([a],[(Integer,a)])
    nextStep = liftA2 (,) ( join ( dropWhile . (==) . head ) . fst) (  (. liftA2 (,) genericLength head) . flip (:)  . snd  <*> join (takeWhile . (==) . head)  .  fst )
--using until with splitAt and findIndex
