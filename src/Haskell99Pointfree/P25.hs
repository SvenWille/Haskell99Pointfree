{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P25
    (
    ) where


import Control.Monad.Fix (fix)
import System.Random (randomR, newStdGen)
import Control.Applicative ((<*>),liftA2)
import Control.Monad (join , liftM4, ap)
import Control.Lens


--using until
p25_1 :: [a] -> IO [a]
p25_1 =  ( newStdGen <&> ) .  ((view _2 . until condition nextStep) . ) . ap (,[],,)  ( subtract 1 . length )    --first postition is the actual list , second is the new list ,
  where                                                                                              --third contains the length  and fourth will contain the "Gen"
    condition = null . view _1
    nextStep  =  over _3 (subtract 1) .  join (( . snd) .  set _4 . view (_1._2)).  ( ap (liftA2 (++) . take) (drop. (+1)) . view (_1._1)  >>= over (_2._1) )  . ( ((:) . liftA2 (!!) (view (_2._1) ) (view (_1._1))) >>= over (_2._2) )  . join ((,) . liftA2  (randomR . (0,)) (view _3) (view _4))

{-
--using take, nub and map
p25_2 :: [a] -> [a]
p25_2 =
-}

{-
--using foldl and nub
p25_3 :: [a] -> [a]
p25_3 =
-}
