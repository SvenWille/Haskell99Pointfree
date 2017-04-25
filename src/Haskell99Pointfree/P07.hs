{-# LANGUAGE DeriveDataTypeable , TemplateHaskell, TupleSections #-}
module Haskell99Pointfree.P07
    (
    ) where


import Control.Lens (makePrisms , preview , over , _1)
import Control.Lens.Extras (is)
import Control.Monad.Extra (ifM)
import Data.DeriveTH (makeIs, makeFrom, derive )
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.Data
import Data.Typeable

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

--using the derive library
data NestedList3 a = List3 [NestedList3 a] | Elem3 a

$(derive makeIs ''NestedList3 )
$(derive makeFrom ''NestedList3 )

--using a stack (which is basically the build-in list)
p07_3 :: NestedList3 a -> [a]
p07_3 =  reverse . snd . until (null . fst ) (ifM (isElem3 . head . fst) buildTupleElem buildTupleList) . (,[])  . (:[])
  where

    --this branch is taken if the top element from the stack is an element (Elem3)
    buildTupleElem :: ([NestedList3 a],[a]) -> ([NestedList3 a],[a])
    buildTupleElem = liftA2 (,) (tail . fst)  (liftA2 (:) (fromElem3 . head . fst) snd )

    --this branch is taken if the top element from the stack is a List3 (a nested list)
    buildTupleList :: ([NestedList3 a],[a]) -> ([NestedList3 a],[a])
    buildTupleList =  over _1 (liftA2 (++) ( fromList3 . head ) tail)

{-
--using data and typeable
data NestedList4 a = List4 [NestedList4 a] | Elem4 a deriving (Data , Typeable)

--using fix tailrecursively
p07_4 :: NestedList4 a -> [a]
p07_4 =  fix ()

-}
