module Haskell99Pointfree.P21
    (p21_1
    ) where

import Control.Applicative
import Control.Lens

--simple solution
p21_1 :: a -> [a] -> Int -> [a]
p21_1 = ( . ( (. subtract 1)  . flip splitAt ) ) . (.) . liftA2 (++) fst . ( . snd)  .  (:)


--using take and drop
p21_2 :: a ->  [a] -> Int -> [a]
p21_2 =  ( . ( ( liftA2 (,) head last .  ) .  (. flip ($). subtract 1)  . flip map .  flip map (map flip[ take ,  drop]) . flip ($) ) ) . (.) . liftA2 (++) fst . (. snd) . (:)


--using until
