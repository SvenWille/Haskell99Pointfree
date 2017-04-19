{-# LANGUAGE DeriveDataTypeable #-}
module Haskell99Pointfree.P07
    (
    ) where

--using lebels
data NestedList a = NestedList {children ::[NestedList a]} | Elem {val::a}

--recursive
p07_1 :: NestedList a -> [a]
p07_1 = concatMap

--using prisms
data NestedList2 a = NestedList2 [NestedList2 a] | Elem2 a

--using a stack (which is the build in list)
p07_2 :: NestedList a -> [a]
p07_2 =   until (null . fst ) () . (,[])
