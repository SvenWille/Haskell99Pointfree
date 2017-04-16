module Haskell99Pointfree.P20
    (p20
    ) where

--import Control.Monad.Fix
import Control.Lens
import Control.Applicative
import Data.List
import Control.Monad.Extra
import Data.Bool.HT

--unsafe for index <= 0
p20 :: [a] -> Int -> (a , [a])
p20 =  ( liftA2 (,) (last . fst) (liftA2 (++) (init . fst ) snd) . ) . flip splitAt

{-
--using lenses, still unsafe
p20_1 :: [a] -> Int -> (a,[a])
p20_1 =  ( liftA2 (,) (last . fst) (liftA2 (++) (init . fst ) snd) . ) . flip splitAt
-}

--using find,filter and zip, safe, does not change the list in case of invalid index
p20_2 :: [a] -> Int -> (Maybe a,[a])
p20_2 =  ( .  liftA2 (,) ( (fmap snd . ) . find  .  ( . fst)  . (==))  ( (map snd . ) . filter . ( . fst) . (/=) )  ) . flip (uncurry (liftA2 (,))) . zip [1..]


--using foldr
p20_3 :: [a] -> Int -> (Maybe a , [a])
p20_3 = ( . ( flip foldr (Nothing,[]) . ($  ( ( . snd ) . (,) . Just . snd))  . ($ ( join . ( ( . fst) . )  . (  . snd ) . (flip (,) . )  . (:) . snd) ) . liftA3 if' . ( . fst)  . (/=) )) .flip id . zip [1..]
