{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P4
    ( p4 , p4', p4'', p4''', p4'''', p4_5
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.Bool.HT


--for functions pure is the same as const
p4 :: [a] -> Integer
p4 = foldl ( pure . (+1)) 0


p4' ::[a] -> Integer
p4' = fst . until (null . snd) (liftM2 (,) ((+1) . fst) (tail . snd)) .  (0,) --without tuple sections it would be (,) 0

--return is the same as const when used for functions
--fmap is the same as map for lists
p4'' :: [a] -> Integer
p4'' = sum . fmap (return 1)

p4''' :: [a] -> Integer
p4''' = ifM  (not . null) ( p4''' . tail)  (const (-1)) >>= ( const . (+1) )

--variant of p4'''
p4'''' :: [a] -> Integer
p4'''' =  ifM null (const 0) ((+1) . p4'''' . tail)

--variant of p4'''' not using ifM
p4_5 :: [a] -> Integer
p4_5 =   liftM3 if' null (const 0) ((+1) . p4_5 . tail)
