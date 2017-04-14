module Haskell99Pointfree.P06
    ( p06, p06', p06'', p06''', p06_4, p06_5
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.List


p06, p06', p06'', p06''', p06_4, p06_5 :: Eq a => [a] -> Bool


p06 = null .  until(liftA2 (||) null (liftA2 (/=) head last)) (init . tail)


p06' = undefined  -- and .  mapAccumR


p06'' = and . liftA2 (zipWith (==)) id reverse


p06''' = ap (==) reverse


p06_4 = undefined -- join (foldl ( (==) . head ))


p06_5 =    join ( ( . ( uncurry( (==) . reverse ) . ) ) . ifM ( odd . length)   . (uncurry(flip(  (==) . reverse . tail )) . ) )  (join (splitAt . flip div 2 . length) )
