module Haskell99Pointfree.P19
    (
    ) where

import Control.Applicative ((<*>), liftA2)
import Data.Tuple (swap)
import Control.Monad.Extra (ifM)
import Data.Function ((&))
import Control.Monad (join, ap)

p19_1 :: [a] -> Int -> [a]
p19_1 = ( . )  . ( ( uncurry (++) .swap) . )  . flip splitAt <*> flip mod . length


p19_2 :: [a] -> Int -> [a]
p19_2 =  liftA2 ((<*>) . (ifM ( < 0) . ))  (  ((reverse .) . ) . ( ( . abs) . ) . ( . reverse)) id  shifter
  where
    shifter :: [a] -> Int -> [a]
    shifter = ap (( .  )  . flip ( join (( . take) . liftA2 (++) .  drop)) ) (flip mod  . length )
