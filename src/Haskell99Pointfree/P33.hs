module Haskell99Pointfree.P33
    (p33, p33', p33''
    ) where


import Control.Monad

p33 :: Int -> Int -> Bool
p33 = ((==) 1 . ) . gcd

--using own gcd version
p33' :: Int -> Int -> Bool
p33' = undefined

p33'' :: Int -> Int -> Bool
p33'' = ap (const (== 1) ) . gcd 
