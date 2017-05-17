{-# LANGUAGE LambdaCase , TupleSections #-}
module Haskell99Pointfree.P96
    (
    ) where

import Control.Monad.Extra (ifM)
import Data.Char
import Control.Applicative ((<*>) , liftA3)
import Data.Maybe
import Data.List
import Control.Lens
import Control.Monad

--using foldl
p96_1 :: String -> Bool
p96_1 =  ifM null (const False) (ifM ( isLetter . head ) ( ( ifM ( (=='-') . fst) (const False) snd . ) . foldl foldHelper . (,True) . head <*> tail ) (const False) )
  where
    foldHelper =  ap  .  ( ( ( . (&&) ) . flip (over _2))  . )  . flip (set _1)   <*> (flip any conditions . ) . ( flip ($) . ) . (,) . fst
      where
        conditions =   map uncurry [ ( . (flip any [isDigit, isLetter, (== '-')] . flip ($) ))  . (&&) .  isLetter , ( . (flip any [isDigit, isLetter, (== '-')] . flip ($) ))  . (&&) .  isDigit , ( . (flip any [isDigit, isLetter] . flip ($) ))  . (&&) .  (=='-') ]
        {-conditions =   map uncurry [ (\ a b -> isLetter a && (isDigit b || isLetter b || b == '-') ) , (\a b -> isDigit a && (b == '-' || isLetter b || isDigit b)) , (\a b -> (a == '-' && (isAlpha b || isDigit b)))  ]-}

--if we allow lambda case
{-
p96 :: String -> Bool
p96 = until (uncurry(  ( . not )  .  (||) . null)) () .  (,True)
-}

-- using regex
