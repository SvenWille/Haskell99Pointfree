module Haskell99Pointfree.P96
    (
    ) where

{-
p96 :: String -> Bool
p96 = until (uncurry(  ( . not )  .  (||) . null)) () .  (,True)
-}
