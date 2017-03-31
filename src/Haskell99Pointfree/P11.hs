module Haskell99Pointfree.P11
    (p11
    ) where

import Data.List
import Data.Bool.HT
import Control.Applicative
import Control.Monad

data MorS a = Multiple Integer a  | Single a deriving Show

p11, p11', p11'', p11''' :: Eq a => [a] ->  [MorS a]


p11 = map (liftA3 ifThenElse ( (==1) . genericLength) (Single . head) (liftA2 Multiple genericLength head)) . group

p11' = undefined -- map (join ( ( . join ( ( . head ) . Multiple .genericLength) ) .  ifThenElse  . (==1) . genericLength)) . group

p11'' =  undefined -- flip (flip ifThenElse)

p11''' = undefined
