{-# LANGUAGE DeriveDataTypeable #-}
module Haskell99Pointfree.P07
    (
    ) where


data NestedList a = NestedList {children ::[NestedList a]} | Elem {val::a}


p07_1 :: NestedList a -> [a]
p07_1 =
