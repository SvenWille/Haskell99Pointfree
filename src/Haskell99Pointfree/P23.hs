{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P23
    (
    ) where

import System.Random
import Control.Applicative
import System.IO.Unsafe
import Control.Monad


p23 :: [a] -> Int ->  [a]
p23 =   (   $ (unsafePerformIO getStdGen)) . ap (  (.) .  foldl ( ( . (flip (:)) ) . flip (.)  .  (!!)  )  . (,[])   )   ( ( . take  )  . flip (.) .  randomRs . (0,) .length )

{-

p23 :: []
-}
