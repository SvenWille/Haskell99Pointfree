{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P23
    (p23_1
    ) where

import System.Random
import Control.Applicative
import System.IO.Unsafe --only used in p23_2
import Control.Monad
import Control.Arrow
import Data.List
import Data.Bool.HT (if')

--version where parts of the function are split up for readability
--this version allows multiple draws
p23_1 :: [a] -> Int -> IO [a]
p23_1 =   (fmap snd . )  .   ap (( . helper1 ) . (.) .  fmap . foldl  helper3 . (,[])) helper2
  where
    helper1 :: IO [Int] -> Int -> IO [Int]
    helper1  =  ( . take ) .  flip fmap

    --first argument for "ap" in p23. Generates a list of indices, depending on the length of the input array
    helper2 :: [a] -> IO [Int]
    helper2 =  ( `liftM` newStdGen) . randomRs . (0,) . subtract 1 . length

    helper3 :: ([a],[a]) -> Int -> ([a],[a])
    helper3 = ( (&&&) (fst . fst)  (uncurry( uncurry (flip (.)) . (***) (!!)   (flip (:)))) . ) . (,)


-- version using take and nub, If the index is too large or negative , Nothing is returned
p23_2 :: [a] -> Int -> IO (Maybe [a])
p23_2  = liftA2 (`if'` return  Nothing) . liftA2 (||) (<0) . (<) . length <*> trueBranch
  where
    trueBranch = ((( flip fmap  newStdGen . ) . ( . (take:: Int -> [a] -> [a]))) . ) . ( . (((Just .) . ) . (.) )) . flip (.) . flip ( . ) . (nub . ) . randomRs . (0,) . subtract 1 . length <*>  map . (!!)


--alternative version using unsafePerformIO (only useful for learning purposes, since unsafePerformIO doesn't reevaluate getStdGen which results in getting the same )
p23_3 :: [a] -> Int ->  [a]
p23_3 = ((reverse . snd ) . ) . ap (  (.) . foldl  (uncurry( ap ( ( . ) . (.) . (,) ) ( ( . flip (:) ) . flip (.) . (!!)  ))) . (,[])) ( ( . take  ) . flip id .   flip randomRs (unsafePerformIO newStdGen) . (0,) . subtract 1 . length )
