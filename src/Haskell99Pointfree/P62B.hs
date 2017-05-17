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

p62B_1 :: Tree a -> Int -> [a]
p62B_1 =  ( ($ 1) . ) . flip (mfix mfixFn )
  where
    mfixFn recc goal tr curr = if is _Empty tr then [] else  (if goal == curr then [val tr] else  (flip recc (curr + 1)) (lft tr) ++   (flip recc (curr + 1)) (rgt tr)   )
