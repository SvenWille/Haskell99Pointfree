module Haskell99Pointfree.P22
    (p22_1, p22_2 --, p22''
    ) where

import Control.Applicative
import Data.Bool.HT
--trivial soution, ignoring descending cases
p22_1 :: Integer -> Integer -> [Integer]
p22_1 = enumFromTo

--imporved version of p22_1 where descending cases are taken into account
p22_2 :: Int -> Int -> [Int]
p22_2 =  (  liftA3 ifThenElse (uncurry (<)) (uncurry enumFromTo)  (reverse . uncurry (flip enumFromTo)) . ) . (,)
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
