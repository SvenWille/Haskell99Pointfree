{-# LANGUAGE DeriveDataTypeable , TemplateHaskell #-}
module Haskell99Pointfree.P07
    (
    ) where


import Control.Lens (makePrisms , preview)
import Control.Lens.Extras (is)
import Control.Monad.Extra (ifM)
import Data.DeriveTH (makeIs )
import Data.Maybe (fromJust)

--using Prisms
data NestedList a = List [NestedList a] | Elem a

makePrisms ''NestedList

--recursive
p07_1 :: NestedList a -> [a]
p07_1 =  ifM (is _Elem) ( (:[]) . fromJust . preview _Elem) (concatMap p07_1 . fromJust . preview _List)


--the same using record labels
data NestedList2 a = List2 {children :: [NestedList2 a]} | Elem2 {val :: a}

makePrisms ''NestedList2

p07_2 :: NestedList2 a -> [a]
p07_2 =  ifM (is _Elem2) ( (:[]) . val) (concatMap p07_2 . children)
{-
--using the derive library
data NestedList2 a = List2 [NestedList2 a] | Elem2 a

$(derive makeIs ''NestedList2 )

--using a stack (which is the build-in list)
p07_2 :: NestedList a -> [a]
p07_2 =   until (null . fst ) () . (,[])
-}
