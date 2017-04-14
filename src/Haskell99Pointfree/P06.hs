{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P06
    ( p06, p06', p06'', p06''', p06_4, p06_5, p06_6
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.List
import Control.Arrow
import Data.Bool.HT

p06, p06', p06'', p06''', p06_4, p06_5, p06_6 :: Eq a => [a] -> Bool


p06 = ((<= 1) . length)  .  until(liftA2 (||) (  (<= 1) . length) (liftA2 (/=) head last)) (init . tail)

--using mapAccumL
p06' = and . snd . uncurry (mapAccumL  ( join ( ( . ( head . fst) ) .  (. (==)) . (.)  . (,) . snd)     . splitAt 1 )) .  second reverse . join (liftA3 if' ( odd . length) . (uncurry (flip((,) . tail ))  . ) ) (join(splitAt . flip div 2 . length))

--does redundant comparisons
p06'' = and . liftA2 (zipWith (==)) id reverse

--does redundant comparisons
p06''' = ap (==) reverse

--does unnecessary comparisons, uses arrows
p06_4 =  snd . join (foldl ( ( .  ( (uncurry (&&) . ) . first .  (. head)  . (==) )   )  .  flip ( (tail . fst) &&&) )  . (,True)  .  reverse)


p06_5 =    join ( ( . ( uncurry( (==) . reverse ) . ) ) . ifM ( odd . length)   . (uncurry(flip(  (==) . reverse . tail )) . ) )  (join (splitAt . flip div 2 . length) )


p06_6  = uncurry isPrefixOf . second reverse .  ap (flip splitAt) (flip div 2 . length)   
