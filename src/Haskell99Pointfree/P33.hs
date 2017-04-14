module Haskell99Pointfree.P33
    (p33, p33', p33'', p33''',p33_4
    ) where


import Control.Monad

p33 :: Int -> Int -> Bool
p33 = ((==) 1 . ) . gcd

--using own gcd version
p33' :: Int -> Int -> Bool
p33' = undefined

p33'' :: Int -> Int -> Bool
p33'' = ap (const (== 1) ) . gcd

p33''' :: Int -> Int -> Bool
p33''' = fmap (== 1) . gcd

p33_4 :: Int -> Int -> Bool
p33_4 = liftM (== 1) . gcd
