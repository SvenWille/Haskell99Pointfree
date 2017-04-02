{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Haskell99Pointfree.P12
    (p12, MorS (..), p12', MorS2(..)
    ) where


import Data.Data
import Control.Monad
import Data.Bool.HT
import Control.Applicative
import Data.Maybe
--import Control.Lens
--import Control.Lens.Extras (is)

--solution with record filed names

data MorS a = Multiple {nm::Int, val::a} | Single {val::a}  deriving (Data,Typeable)

p12= undefined
{-
p12:: Data a => [MorS a] -> [a]
p12 = concatMap (liftA3 ifThenElse (  (==) (toConstr (Single ())) . toConstr )  (replicate 1 . val)  (liftA2 replicate nm val)  )
-}

data MorS2 a = Multiple2 Int a | Single2 a deriving (Data,Typeable)

p12':: Data a => [MorS2 a] -> [a]
p12' = concatMap (liftA3 ifThenElse (  (==) (toConstr (Single2 ())) . toConstr )  ( flip (:) [] . fromJust . gmapQi 0 cast  )  (liftA2 replicate  (fromJust . gmapQi 0 cast)  (fromJust . gmapQi 1 cast))  )

--solution using lenses
{-
data MorS3 a = Multiple3 Int a | Single3 a deriving (Data, Typeable)

makePrisms ''MorS3

p12'' :: Data a => [MorS3 a] -> [a]
p12'' = concatMap (liftA3 ifThenElse ( is _Single3 )  (replicate 1 . fromJust . preview _Single3 )  ( join ( ( . snd) . replicate . fst ) . fromJust  . preview _Multiple3  ))

-}


--solution using the derive and lens library
{-
data MorS3 a = Multiple3 Int a | Single3 a


makePrisms ''MorS3

make

p12'' = concatMap (liftA3 ifThenElse (  (==) (toConstr (Single ())) . toConstr )  (replicate 1 . val)  (liftA2 replicate nm val)  )
-}
--using template haskell to find out whether
