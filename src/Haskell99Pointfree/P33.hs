module Haskell99Pointfree.P33
    (p33_1, p33_2, p33_3, p33_4,p33_5
    ) where


import Control.Monad
import Haskell99Pointfree.P32

p33_1 :: Int -> Int -> Bool
p33_1 = ((==) 1 . ) . gcd

--using own gcd version
p33_2 :: Int -> Int -> Bool
p33_2 = ((==) 1 . ) . p32_1  

p33_3 :: Int -> Int -> Bool
p33_3 = ap (const (== 1) ) . gcd

p33_4 :: Int -> Int -> Bool
p33_4 = fmap (== 1) . gcd

p33_5 :: Int -> Int -> Bool
p33_5 = liftM (== 1) . gcd
