{-# LANGUAGE DeriveDataTypeable #-}
module Haskell99Pointfree.P12
    (p12, MorS (..)
    ) where


import Data.Data
import Control.Monad
import Data.Bool.HT
import Control.Applicative
import Data.Maybe


--solution with record filed names
data MorS a = Multiple {nm::Int, val::a} | Single {val::a}  deriving (Data,Typeable)

p12 :: Data a => [MorS a] -> [a]
p12 = concatMap (liftA3 ifThenElse (  (==) (toConstr (Single ())) . toConstr )  (replicate 1 . val)  (liftA2 replicate nm val)  )


--solution using lenses
--p12' = concatMap ()
