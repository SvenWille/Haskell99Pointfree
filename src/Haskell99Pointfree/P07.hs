{-# LANGUAGE DeriveDataTypeable , TemplateHaskell #-}
module Haskell99Pointfree.P07
    (
    ) where


import Control.Lens (makePrisms)
import Control.Lens.Extras (is)
import Control.Monad.Extra (ifM)

--using Prisms
data NestedList a = List {child ::[NestedList a]} | Elem {val::a}

makePrisms ''NestedList

--recursive
p07_1 :: NestedList a -> [a]
p07_1 =  ifM (is _Elem) ( (:[]) . val) (concatMap p07_1 . child)


--using the derive library
data NestedList2 a = NestedList2 [NestedList2 a] | Elem2 a


{-
--using a stack (which is the build-in list)
p07_2 :: NestedList a -> [a]
p07_2 =   until (null . fst ) () . (,[])
-}
