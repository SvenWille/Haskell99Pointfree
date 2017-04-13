module Haskell99Pointfree.P33
    (p33, p33', p33''
    ) where

import Control.Arrow

p33 :: Int -> Int -> Bool
p33 = ((==) 1 . ) . gcd

--using own gcd version
p33' :: Int -> Int -> Bool
p33' = undefined

--using arrows (redundant nonsense version)
p33'' :: Int -> Int -> Bool
p33'' = ((== 1) <<< ) <<< curry app <<< curry app gcd
