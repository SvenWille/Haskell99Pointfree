{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P13
    (
    ) where


import Control.Applicative (liftA2, liftA3, (<*>))
import Control.Monad (liftM4, join)
import Data.List (genericLength, group)
import Control.Monad.Extra
import Data.Bool.HT
import Control.Lens

data ListItem a = Multiple Integer a  | Single a deriving Show

--even though you are not supposed to create sublists I decided to include one such solution since it creates a "nice to look at" solution
p13_1 :: Eq a => [a] ->  [ListItem a]
p13_1 = map (liftA3 ifThenElse ( (==1) . genericLength) (Single . head) (liftA2 Multiple genericLength head)) . group

--under construction
--using until
--"readable" but redundant version
p13_2 :: Eq a => [a] -> [ListItem a]
p13_2 =  liftA2 (`if'` [] ) null (reverse   . (  (:) . ifM ( (==1) . view _1) (Single . (^._2) ) ( Multiple . (^._1) <*> view _2) <*> view _4) .  join ((until (null . view _3) nextStep . ) . (. tail) . (1,,,[]) . head))
  where
    nextStep :: Eq a => (Integer , a , [a], [ListItem a]) -> (Integer ,a , [a] , [ListItem a])
    nextStep = ifM ( join (( . view _2 ) . (==) . head . view _3 )) ( over _3 tail  . over _1 (+1) ) ( liftA3 (1,,,) (head . view _3) (tail . view _3 ) (liftA2 (:) (ifM ( (==1) . view _1) (Single . view _2) (liftA2 Multiple (^._1) (^._2)) ) (view _4) ) )

{-
p13_3 :: Eq a => [a] -> [ListItem a]
p13_3  = liftA2 (flip if' Nothing) (foldr . )
-}
