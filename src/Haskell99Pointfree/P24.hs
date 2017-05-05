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
--using nub with take
p24_2 :: Int -> Int -> IO (Maybe [Int])
p24_2 = liftM2 ( `if'` return Nothing) . (>) <*>   ( . (randomRs  . (1,))) . (flip fmap newStdGen .) . (.) . ( . nub) . (Just .) . take
{-
--using until
p24_3 :: Int -> Int -> IO (Maybe [Int])
-}
