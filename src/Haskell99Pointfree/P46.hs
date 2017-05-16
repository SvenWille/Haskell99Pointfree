module Haskell99Pointfree.P46
    (
    ) where


import Data.Bool.HT
import Data.Bool
import Control.Monad.Extra (ifM)
import Data.Bifunctor (bimap)
import Control.Applicative (liftA2, (<*>))
import Data.List (intercalate)
import Control.Monad (replicateM)

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


nand_1 :: Bool -> Bool -> Bool
nand_1 = (not_1 . ) . and_1


xor_1 :: Bool -> Bool -> Bool
xor_1 = (not . ) .  (==)

impl_1 :: Bool -> Bool -> Bool
impl_1 =  (||)  . not

equ_1 :: Bool -> Bool -> Bool
equ_1 = (not . ) .  (/=)

equ_2 :: Bool -> Bool -> Bool
equ_2 = liftA2 (||) . (&&)   <*>  ( . not ) . (&&)  . not


truthTable_1 :: (Bool -> Bool -> Bool) -> IO()
truthTable_1 =  putStrLn .  unlines . flip map  (replicateM 2 [True, False])  .  (( unwords . map show) . ) . ( ( .(:[]) ). (++)  <*> ) .  ($ last) .  ($ head) .  liftA2 
