module Haskell99Pointfree.P6
    ( p6,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.List

p6, p6', p6'', p6''', p6_4, p6_5 :: Eq a => [a] -> Bool


p6 = null .  until(liftA2 (||) null (liftA2 (/=) head last)) (init . tail)


p6' = undefined -- and . mapAccumR


p6'' = and . liftA2 (zipWith (==)) id reverse


p6''' = ap (==) reverse


p6_4 = undefined -- join (foldl ( (==) . head ))


p6_5 = undefined
