module Haskell99Pointfree.P7
    (
    ) where


data NestedList a = NestedList [NestedList a] | Elem a


p7 :: NestedList a -> [a]
p7 = undefined
