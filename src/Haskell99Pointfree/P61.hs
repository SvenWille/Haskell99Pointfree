{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P61
    (
    ) where

import Control.Monad.Fix (fix)
import Control.Applicative (liftA2)
import Control.Monad.Extra (ifM)
import Control.Lens (makePrisms)
import Control.Lens.Extras (is)

data Tree a = Empty  | Branch {val :: a , lft :: Tree a, rgt :: Tree a} deriving (Show , Eq)

makePrisms ''Tree
--using fix
p61_1 :: Tree a -> Int
p61_1 = fix (ifM (is _Empty) (const 0) . ifM (liftA2 (&&) (is _Empty . lft) (is _Empty . rgt)) (const 1)  . liftA2 (liftA2 (+)) ( . lft) (  . rgt))

{-
p61_2 :: Tree a -> Int
p61_2 =

--using until with a stack (the built-in list)
p61_2 :: Tree a -> Int
p61_2 =

-}
