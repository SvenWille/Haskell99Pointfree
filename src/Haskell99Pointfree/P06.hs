{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P06
    ( p06_1, p06_2, p06_3, p06_4, p06_5, p06_6, p06_7
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.List
import Control.Arrow
import Data.Bool.HT



p06_1 :: Eq a => [a] -> Bool
p06_1 = ((<= 1) . length)  .  until(liftA2 (||) (  (<= 1) . length) (liftA2 (/=) head last)) (init . tail)

--using mapAccumL
p06_2 :: Eq a => [a] -> Bool
p06_2 = and . snd . uncurry (mapAccumL  ( join ( ( . ( head . fst) ) .  (. (==)) . (.)  . (,) . snd)     . splitAt 1 )) .  second reverse . join (liftA3 if' ( odd . length) . (uncurry (flip((,) . tail ))  . ) ) (join(splitAt . flip div 2 . length))

--does redundant comparisons
p06_3 :: Eq a => [a] -> Bool
p06_3 = and . liftA2 (zipWith (==)) id reverse

--does redundant comparisons
p06_4 :: Eq a => [a] -> Bool
p06_4 = ap (==) reverse

--does unnecessary comparisons, uses arrows
p06_5 :: Eq a => [a] -> Bool
p06_5 =  snd . join (foldl ( ( .  ( (uncurry (&&) . ) . first .  (. head)  . (==) )   )  .  flip ( (tail . fst) &&&) )  . (,True)  .  reverse)

p06_6 :: Eq a => [a] -> Bool
p06_6 =    join ( ( . ( uncurry( (==) . reverse ) . ) ) . ifM ( odd . length)   . (uncurry(flip(  (==) . reverse . tail )) . ) )  (join (splitAt . flip div 2 . length) )

p06_7 :: Eq a => [a] -> Bool
p06_7  = uncurry isPrefixOf . second reverse .  ap (flip splitAt) (flip div 2 . length)
