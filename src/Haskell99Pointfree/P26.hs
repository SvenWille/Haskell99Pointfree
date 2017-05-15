{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P26
    (
    ) where

import Control.Lens
import Data.Maybe (fromJust)
import Control.Monad.Extra (ifM)
import Control.Monad (ap, join)
import Data.List.Extra (uncons)
import Data.Bool (bool)
import Control.Applicative (liftA2)


--using a stack (which is the build in list)
p26_1 :: Int -> [a] -> [[a]]
p26_1 = liftA2 (bool (const []))  (( (reverse .view _4 . until  (null . view _2) nextStep)  . ) .  ( . (:[]) )  .  (,,[],[])) (>=0)
  where
    nextStep = ifM ( (==0) . view _1)  ( over _3 tail . ap (( . (:)). flip (over _4)) (reverse . view _3)  . over _2 tail . set _1 1)  nextStep2
    nextStep2 = ifM  (null . head . view _2) (over _3 tail . over _2 tail . over _1 (+1)) nextStep3
    nextStep3 = ap (uncurry . (( ( . join ( (  . (:) ) . ( . )  . (:)  ) ) . flip (over  _2)) . ) . ( . (:)) . flip (over _3) . over _2 tail ) (fromJust . Data.List.Extra.uncons . head .view _2) . over _1 (subtract 1)
