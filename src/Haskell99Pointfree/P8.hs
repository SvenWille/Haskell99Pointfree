module Haskell99Pointfree.P8
    (
    ) where

import Data.List

p8, p8' :: Eq a => [a] -> [a]


p8 = map head . group

p8' = undefined -- foldr () . 
