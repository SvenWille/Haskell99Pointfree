module Haskell99Pointfree.P19
    (
    ) where

import Control.Applicative ((<*>))
import Data.List
import Data.Tuple

p19_1 :: [a] -> Int -> [a]
p19_1 = ( . )  . ( ( uncurry (++) .swap) . )  . flip splitAt <*> flip mod . length
