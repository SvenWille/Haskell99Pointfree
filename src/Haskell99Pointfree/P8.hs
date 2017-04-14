module Haskell99Pointfree.P8
    (
    ) where

import Data.List
import Control.Applicative
import Control.Monad
import Data.Bool.HT

p8, p8' :: Eq a => [a] -> [a]


p8 = map head . group

--under construction
p8' =  liftA2 (foldr   (  ( liftM3  if' (uncurry(flip( (==) . fst ))) () ()   . ) . (,) )   )      ((,[]) . head)    tail
