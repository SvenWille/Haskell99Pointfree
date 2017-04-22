{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P11
    (p11_1
    ) where

import Data.List
import Data.Bool.HT
import Control.Applicative
import Control.Monad
import Haskell99Pointfree.P10
import Control.Monad.Extra

data ListItem a = Multiple Integer a  | Single a deriving Show

p11_1 :: Eq a =>  [a] -> [ListItem a]
p11_1 =  map ( ifM ( (==1) . fst) ( Single . snd) (liftA2 Multiple fst snd) ) .  p10_1

p11_2 :: Eq a => [a] -> [ListItem a]
p11_2 = reverse . snd . until (null . fst) (liftA2 (,) (tail . fst)  ( ( (:) . liftM3 if' ((==1) . fst)  (Single . snd) (Multiple . fst <*> snd ) . head . fst )  <*> snd  ) ) .  (,[]) . p10_2
