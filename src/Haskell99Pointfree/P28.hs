module Haskell99Pointfree.P28
    (
    ) where

import Data.Function
import Data.List
import Control.Monad.Extra
import Control.Applicative
import Data.List.Extra (uncons)
import Data.Maybe
import Data.Ord


p28_1 :: [[a]] -> [[a]]
p28_1 = sortOn length


p28_2 :: [[a]] -> [[a]]
p28_2 = sortBy (compare `on` length)

{-
--using our own quicksort
p28_3 :: [[a]] -> [[a]]
p28_3 = ifM null id    (   uncurry (liftA3   liftA2  (((++) . ) . (:)) . join ( ) (   ) . fromJust . uncons  )
-}
