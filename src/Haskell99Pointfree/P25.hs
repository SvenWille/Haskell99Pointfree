module Haskell99Pointfree.P25
    (
    ) where


import Control.Monad.Fix (fix)
import System.Random (randomR, newStdGen)
import Control.Applicative ((<*>),liftA2)
import Control.Monad (join , liftM4)
import Control.Lens
{-
--using until
p25_1 :: [a] -> [a]
p25_1 =  ( <$> newStdGen) .  until condition nextStep . (,[],,) --first postition is the actual list , second is the new list ,
  where                                                         --third contains the length  and fourth will contain the "Gen"
    condition = null . view _1
    nextStep  =  join   <*> liftA2  (randomR . (0,)) (view _3) (view _4)
-}
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
