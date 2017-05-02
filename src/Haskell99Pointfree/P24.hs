{-# LANGUAGE TupleSections #-}
module  Haskell99Pointfree.P24
    (
    ) where


import System.Random
import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Bool.HT
import Control.Monad
import Data.List (nub)
{-
--complex solution
p24_1 :: Int -> Int -> IO (Maybe [Int])
p24_1 = liftA2 (flip if' (return Nothing)) . (>) <*>   (( Just . view _3 . flip fmap newStdGen) .) . ((unitl  ((== 0) . (^._1)) nextStep . ) .)   . (,,[],)
  where
    nextStep = ap (liftM3 ( . (,,,)) (^._1)  ) ( liftA2 randomR  ( (0,) . subtract 1 . length . view _2)  (view _4) ) . over _1 (subtract 1)
-}

--using nub with take
p24_2 :: Int -> Int -> IO (Maybe [Int])
p24_2 = liftM2 ( `if'` return Nothing) . (>) <*>   ( . (randomRs  . (1,))) . (flip fmap newStdGen .) . (.) . ( . nub) . (Just .) . take 
{-
--using until
p24_3 :: Int -> Int -> IO (Maybe [Int])
-}
