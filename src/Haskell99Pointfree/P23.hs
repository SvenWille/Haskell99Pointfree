{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P23
    (p23, p23'
    ) where

import System.Random
import Control.Applicative
import System.IO.Unsafe
import Control.Monad
import Control.Arrow
import Data.List


p23 :: [a] -> Int -> IO [Int]
p23 =    ap (( . ( ( . take ) .  ( . flip fmap ))) . (.) . (<$>)  . snd . foldl ((( (fst . fst)  &&&   ( uncurry (flip(.)) . uncurry( ((!!):: [a] -> Int -> a) ***  flip (:) ))) . ) . (,):: (([a],[a]) -> Int -> (([a],[a]),Int)))
          . (,[])::([a]-> ([a],[a])) ) ( ( `liftM` newStdGen) . randomRs . (0,) . subtract 1 . length)

--alternative version using unsafePerformIO (only useful for learning purposes, since unsafePerformIO doesn't reevaluate getStdGen which results in getting the same )
p23' :: [a] -> Int ->  [a]
p23' = ((reverse . snd ) . ) . ap (  (.) . foldl  (uncurry( ap ( ( . ) . (.) . (,) ) ( ( . flip (:) ) . flip (.) . (!!)  ))) . (,[])) ( ( . take  ) . flip id .   flip randomRs (unsafePerformIO newStdGen) . (0,) . subtract 1 . length )


-- version using
p23'' :: [a] -> Int -> [a]
p23'' = undefined
