{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P08
    (p08_1, p08_2
    ) where

import Data.List
import Control.Applicative
import Control.Monad
import Data.Bool.HT
import Data.Function
import Control.Monad.Fix


p08_1 :: Eq a => [a] -> [a]
p08_1 = map head . group

p08_2 :: Eq a => [a] -> [a]
p08_2 = ap (flip ifThenElse [] . null ) ( snd . join (foldr (join(( . join (( . (( . snd) . (:)) ) . (.) . (,))) . ( $ id) . liftA3 if' . ( . fst) . (==))) . liftA2 (,) id (:[]) . head ))


--variation on p08_2 using foldl instead of foldr

--using until with takewhile and dropwhile
p08_4 :: Eq a => [a] -> [a]
p08_4 = reverse . snd . until ( null . fst ) (liftA2 (,) ( join (dropWhile . (==) . head) . fst) (liftA2 (:) (head . fst)  snd ) )  . (,[])

--using fix with span
p08_5 :: Eq a => [a] -> [a]
p08_5 = fix ( liftA3 if' ( null . fst ) (reverse . snd)  . ( . nextStep ))  . (,[])
  where
    nextStep :: Eq a =>  ([a],[a]) -> ([a],[a])
    nextStep = ((==) . head . fst)  >>= ( . fst) . span >>= ap (( . ) . (,) . snd ) ( ( . snd) . (:) . head . fst )
