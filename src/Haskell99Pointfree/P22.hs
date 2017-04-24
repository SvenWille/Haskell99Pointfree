module Haskell99Pointfree.P22
    (p22, p22' --, p22''
    ) where

import Control.Applicative
import Data.Bool.HT
--trivial soution, ignoring descending cases
p22 :: Integer -> Integer -> [Integer]
p22 = enumFromTo

--imporved version of p22 where descending cases are taken into account
p22' :: Int -> Int -> [Int]
p22' =  (  liftA3 ifThenElse (uncurry (<)) (uncurry enumFromTo)  (reverse . uncurry (flip enumFromTo)) . ) . (,)
{-
--complicated version of p22' without "uncurry" and without "<*>""
p22'' :: Int -> Int  -> [Int]
p22'' =

--version using "<*>"
p22''' :: Int -> Int -> [Int]
p22''' =
-}
{-
--using until
p22_4 ::
-}
