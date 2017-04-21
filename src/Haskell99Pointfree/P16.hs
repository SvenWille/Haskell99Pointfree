module Haskell99Pointfree.P16
    ( p16'''
    ) where

import Data.Bool.HT (if')
import Control.Applicative (liftA3, (<*>))
import Control.Lens
import Control.Monad.Extra (ifM)
import Control.Monad (ap)

--Problem 16: drop every nth element from a list
{-
p16 :: [a] -> Int -> [a]
p16 =  (  .  flip (( , , ) 1) [])  .  flip (foldl (liftA3 if' () () ()  ))
-}
--more elegant version with ifM

{-
--using zip and foldl
p16'' :: [a] -> Int -> [a]
p16'' =
-}

-- using filter with cycle [1,2,3] and zip, if n is negative or zero  , no element will be dropped
p16''' :: [a] -> Int -> [a]
p16''' =  (map fst . ) . ap (filter . ( . snd) . (/=) )  . ( . (cycle . enumFromTo (1::Int) . max 1 )) .  zip

{-
--variation of p16''' returning Nothing on invalid integer input (like zero or negative numbers)
p16_5 :: [a] -> Int -> Maybe [a]
p16_5 = flip (   join  (  (  .     join    (zip . (cycle . enumFromTo (1::Int)  ))  )  .  (flip id) (const Nothing)   . if'  . (>=) 0 ))

--simler version of p16_5
p16_6 :: [a] -> Int -> Maybe [a]
p16_6 =
-}
