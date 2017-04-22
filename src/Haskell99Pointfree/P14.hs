module Haskell99Pointfree.P14
    ( p14_1
    ) where


import Control.Monad (liftM2)
import Control.Monad.Extra (ifM)
import Control.Monad.Fix

p14_1 :: [a] -> [a]
p14_1 = concatMap (replicate 2)


p14_2 :: [a] -> [a]
p14_2 = concat . foldr ( (:) . replicate 2) []

--using fix
p14_3 :: [a] -> [a]
p14_3 = fix ( ifM null id   .  liftM2  (++) (replicate 2 . head)   . ( . tail))

--doing concat only once
p14_4 :: [a] -> [a]
p14_4 =concat . fix ( ifM null (const [])   .  liftM2  (:) (replicate 2 . head)   . ( . tail))
