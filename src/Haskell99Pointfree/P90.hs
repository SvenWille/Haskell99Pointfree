module Haskell99Pointfree.P90
    (
    ) where

import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)

p90_1 :: Int -> [[Int]]
p90_1 = ifM (< 1) trueBranch (const False)
  where
    trueBranch =   until ()  nextStep   . (,[],[]) . enumFromTo 1
      where
        nextStep =
