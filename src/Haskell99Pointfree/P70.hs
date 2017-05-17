{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P70
    (
    ) where

import Control.Monad.Fix (fix)
import Control.Lens

data Tree a = Node a [Tree a] deriving (Eq, Show)

makePrisms ''Tree

p70_1 :: String -> Tree a
p70_1 = fix fixFn
  where
    fixFn recc str = if head str == '^' then Node  else ()  
