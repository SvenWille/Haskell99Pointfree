{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P04
    ( p04 , p04', p04'', p04''', p04'''', p04_5, p04_6, p04_7
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.Bool.HT
import Control.Monad.Fix


--for functions pure is the same as const
p04 :: [a] -> Integer
p04 = foldl ( pure . (+1)) 0


p04' ::[a] -> Integer
p04' = fst . until (null . snd) (liftM2 (,) ((+1) . fst) (tail . snd)) .  (0,) --without tuple sections it would be (,) 0

--return is the same as const when used for functions
--fmap is the same as map for lists
p04'' :: [a] -> Integer
p04'' = sum . fmap (return 1)

p04''' :: [a] -> Integer
p04''' = ifM  (not . null) ( p04''' . tail)  (const (-1)) >>= (const .  (+1) )

--variant of p4'''
p04'''' :: [a] -> Integer
p04'''' =  ifM null (const 0) ((+1) . p04'''' . tail)

--variant of p4'''' not using ifM
p04_5 :: [a] -> Integer
p04_5 =   liftM3 if' null (const 0) ((+1) . p04_5 . tail)

p04_6 :: [a] -> Int
p04_6 = sum . map (const 1)

--using fix
p04_7 :: [a] -> Int
p04_7 = fix (ifM null (const 0) . ( . tail) . ((+1). ))
