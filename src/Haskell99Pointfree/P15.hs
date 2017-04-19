--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P15
    ( module Haskell99Pointfree.P15
    ) where

import Data.Function ((&))
import Control.Applicative
import Control.Monad

--the "&" is almost the same as "$" but with arguments flipped (but is precedece is one higher)
p15_1 :: [a] -> Int -> [a]
p15_1 =  concatMap . replicate & flip


--using foldr
p15_2 :: [a] -> Int -> [a]
p15_2 =  ( . (,[]) ) .  flip ( ( (concat  . snd) .  ) . foldr ( ap  (  (,)  . fst )    .  liftA2 (flip (:)) snd . ( . fst)  . flip replicate ))

--using foldl to show



--using until




{-
import Data.Generics

getField :: (Data r, Typeable v) => Int -> r -> Maybe  v
getField i r = gmapQi i ( cast ) r
-}
