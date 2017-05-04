{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P31
    (
    ) where

import Control.Lens
import Control.Applicative (liftA2, liftA3, (<*>))
import Data.Bool.HT
import Control.Monad (join)


--the obvious and simple method (testing if any number until from 2 to sqrt(n) does divide the input without rest)
p31_1 :: Int -> Bool
p31_1 = liftA3 if' (< 3) (== 2) $ not . view _2 . until condition nextStep . join ((2,False,,) . ceiling .sqrt. fromIntegral)
  where
    nextStep = (over _1 (+1) .) . flip (set _2)  <*>  (== 0) . liftA2  mod (^._4) (^._1)

    condition = liftA2 (||) (view _2) ( (>) . view _1 <*>  view _3)


--using dropWhile
p31_2 :: Int -> Bool
p31_2 = liftA3 if' (< 4)  (1 <)   $ ( (/= 0) . )  . ( . head) . mod   <*> flip dropWhile [2 ..] . liftA2 (liftA2 (&&)) ( (>=) . ceiling . sqrt . fromIntegral) (( (/=0) . ) . mod )

{-
--sieve of turner (simple, unperfomant)
p31_3 :: Int -> Bool
p31_3 = dropWhile
  where
    primes = sieve [2..]
-}

--sieve of erastotenes
