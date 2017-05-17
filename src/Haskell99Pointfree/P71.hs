{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P71
    (
    ) where


import Control.Monad.Fix (fix)
import Control.Lens.Prism
import Control.Lens
import Data.Maybe
import Control.Applicative


data Tree a = Node a [Tree a] deriving (Eq, Show)

makePrisms ''Tree


p71_1 :: Tree a -> Int
p71_1 = fix fixFn 0
  where
    fixFn  = ((( . ( view _2 .fromJust . preview _Node)) . ) . ( . ) . ( . sum ) . (+) <*> ) . (map . ) . ( . (+1))
