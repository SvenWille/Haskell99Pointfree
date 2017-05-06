{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P04
    ( p04_1 , p04_2, p04_3, p04_4, p04_5, p04_6, p04_7, p04_8
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.Bool.HT
import Control.Monad.Fix


--for functions pure is the same as const
p04_1 :: [a] -> Integer
p04_1 = foldl ( pure . (+1)) 0


p04_2 ::[a] -> Integer
p04_2 = fst . until (null . snd) (liftM2 (,) ((+1) . fst) (tail . snd)) .  (0,) --without tuple sections it would be (,) 0

--return is the same as const when used for functions
--fmap is the same as map for lists
p04_3 :: [a] -> Integer
p04_3 = sum . fmap (return 1)

p04_4 :: [a] -> Integer
p04_4 = ifM  (not . null) ( p04_4 . tail)  (const (-1)) >>= (const .  (+1) )

--variant of p4'''
p04_5 :: [a] -> Integer
p04_5 =  ifM null (const 0) ((+1) . p04_5 . tail)

--variant of p4'''' not using ifM
p04_6 :: [a] -> Integer
p04_6 =   liftM3 if' null (const 0) ((+1) . p04_6 . tail)

p04_7 :: [a] -> Int
p04_7 = sum . map (const 1)

--using fix from Control.Monad.Fix
p04_8 :: [a] -> Int
p04_8 = fix (ifM null (const 0) . ( . tail) . ((+1). ))

--using the list Monad
p04_9 :: [a] -> Int
p04_9 =  sum . flip (>>) [1] 
