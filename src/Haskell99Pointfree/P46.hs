module Haskell99Pointfree.P46
    (
    ) where


import Data.Bool.HT
import Data.Bool


not_1 :: Bool -> Bool
not_1 =  ($ True) . ($ False)  .  if'

not_2 :: Bool -> Bool
not_2 = bool True False


--avoiding (&&)
and_1 :: Bool -> Bool -> Bool
and_1 =  ( not_1 . ) .  (  . not_1)  .  (||) . not_1

and_2 :: Bool -> Bool -> Bool
and_2 =  ( and  . ) . flip (:) . (:[])

--avoiding (||)
or_1 :: Bool -> Bool -> Bool
or_1 =  ( not_1 . )  . ( . not_1) . (&&) . not_1   
