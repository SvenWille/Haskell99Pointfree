{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P17
    (
    ) where

import Control.Applicative (liftA2,(<*>))
import Data.Function ((&))
import Control.Lens
import Control.Monad (liftM4 , ap)

--trivial solution
p17_1 :: [a] -> Int -> ([a],[a])
p17_1 = flip splitAt

--take and drop
p17_2 :: [a] -> Int -> ([a],[a])
p17_2 =  (liftA2 (,) . take) <*> drop & flip

--using until
p17_3 :: [a] -> Int -> ([a],[a])
p17_3 = ((liftA2 (,) (reverse . (^._3)) (^._4)   . until (ap ( (||) . null . (^._4)) (ap ( (||) .(< 0) . (^._2) )  (liftA2 (==) (^._1) (^._2)))) (liftM4 (,,,) ( (+1) . (^._1)) (^._2) (ap ( flip (:) . (^._3)) (head . (^._4)) ) (tail . (^._4) ) )) . ) . flip (0 ,,[],)
