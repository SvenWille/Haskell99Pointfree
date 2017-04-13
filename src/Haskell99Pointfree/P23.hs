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

--version where parts of the function are split up for readability
p23 :: [a] -> Int -> IO [a]
p23 =   (fmap snd . )  .   ap (( . helper1 ) . (.) .  fmap . foldl  helper3 . (,[])) helper2

helper1 :: IO [Int] -> Int -> IO [Int]
helper1  =  ( . take ) .  flip fmap

--first argument for "ap" in p23. Generates a list of indices, depending on the length of the input array
helper2 :: [a] -> IO [Int]
helper2 =  ( `liftM` newStdGen) . randomRs . (0,) . subtract 1 . length

helper3 :: ([a],[a]) -> Int -> ([a],[a])
helper3 = ( (&&&) (fst . fst)  (uncurry( uncurry (flip (.)) . (***) (!!)   (flip (:)))) . ) . (,)


-- version using
p23' :: [a] -> Int -> [a]
p23' = undefined


--alternative version using unsafePerformIO (only useful for learning purposes, since unsafePerformIO doesn't reevaluate getStdGen which results in getting the same )
p23'' :: [a] -> Int ->  [a]
p23'' = ((reverse . snd ) . ) . ap (  (.) . foldl  (uncurry( ap ( ( . ) . (.) . (,) ) ( ( . flip (:) ) . flip (.) . (!!)  ))) . (,[])) ( ( . take  ) . flip id .   flip randomRs (unsafePerformIO newStdGen) . (0,) . subtract 1 . length )
