{-# LANGUAGE TemplateHaskell , TupleSections #-}
module Haskell99Pointfree.P72
    (
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.DeriveTH
import Data.Maybe

data Tree a = Node a [Tree a] deriving (Eq, Show)

makePrisms ''Tree

p72_1 :: Tree a -> [a]
p72_1 = snd . until (null . fst) nextStep . ( , [])  . pure
  where
    nextStep = join ( ( . over _1 tail) . liftA2 (.)  (over _2 . (:) . fst) (over _1 . (++) . reverse . snd) . fromJust . preview _Node . head . fst)



data Tree2 a = Node2 a [Tree2 a] deriving (Eq, Show)

$(derive makeFrom ''Tree2)

p72_2 :: Tree2 a -> [a]
p72_2 = liftA2 (++) (concatMap p72_2 . snd) (pure . fst) . fromNode2



--using fix
