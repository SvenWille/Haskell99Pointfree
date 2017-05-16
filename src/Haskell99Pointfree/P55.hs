module Haskell99Pointfree.P55
    (
    ) where

data Tree a = Empty | Branch a (Tree a) (Tree a)

p55_1 :: Int -> [Tree a]
p55_1 = 
