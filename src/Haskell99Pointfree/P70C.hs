module Haskell99Pointfree.P70C
    (
    ) where

import Control.Monad.Fix


data Tree a = Node {val :: a, chlds :: [Tree a]} deriving (Eq, Show)



p70C_1 :: Tree a -> Int
p70C_1 =  fix (  ( (+1) .     ) . ( . chlds ) . (sum . ) . map )
