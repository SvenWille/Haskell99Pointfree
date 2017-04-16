module Haskell99Pointfree.P21
    (p21
    ) where

import Control.Applicative
import Control.Lens

--simple solution
p21 :: a -> [a] -> Int -> [a]
p21 = ( . ( (. subtract 1)  . flip splitAt ) ) . (.) . liftA2 (++) fst . ( . snd)  .  (:)


--using take and drop
p21_1 :: a ->  [a] -> Int -> [a]
p21_1 =  ( . ( ( liftA2 (,) head last .  ) .  (. flip ($). subtract 1)  . flip map .  flip map (map flip[ take ,  drop]) . flip ($) ) ) . (.) . liftA2 (++) fst . (. snd) . (:)


--using until
