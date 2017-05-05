{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P62B
    (
    ) where

import Control.Monad.Fix (mfix)
import Control.Applicative (liftA2, liftA3 , (<*>))
import Control.Monad.Extra (ifM)
import Control.Lens (makePrisms)
import Control.Lens.Extras (is)
import Control.Monad (ap)

data Tree a = Empty  | Branch {val :: a , lft :: Tree a, rgt :: Tree a} deriving (Show , Eq)

makePrisms ''Tree
