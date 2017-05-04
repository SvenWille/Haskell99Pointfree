module Haskell99Pointfree.P46
    (
    ) where


import Data.Bool.HT

not_1 :: Bool -> Bool
not_1 =  ($ True) . ($ False)  .  if'

--avoiding (&&)
and_1 :: Bool -> Bool -> Bool
and_1 =  ( not_1 . ) .  (  . not_1)  .  (||) . not_1
