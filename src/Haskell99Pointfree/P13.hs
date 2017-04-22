{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P13
    (
    ) where


import Control.Applicative
import Control.Monad
import Data.List (genericLength, group)
import Data.Bool.HT
import Control.Lens

data ListItem a = Multiple Integer a  | Single a deriving Show

--even though you are not supposed to create sublists I decided to include one such solution since it creates a "nice to look at" solution
p13_1 :: Eq a => [a] ->  [ListItem a]
p13_1 = map (liftA3 ifThenElse ( (==1) . genericLength) (Single . head) (liftA2 Multiple genericLength head)) . group

--under construction
--using until
p13_2 :: Eq a => [a] -> [ListItem a]
p13_2 =  liftA2 (`if'` [] ) null (reverse . snd . join ((until (null . fst) ( ) . ) . (,1,,[]) . head))
