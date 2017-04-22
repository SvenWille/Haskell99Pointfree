{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P09
    (p09_1 , p09_2, p09_3
    ) where


import Data.List
import Control.Lens
import Data.Bool.HT (if')
import Control.Applicative (liftA2 , liftA3, (<*>))
import Control.Monad ((=<<), join , ap)
import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)
import Data.Function ((&))

--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group

--using foldr
p09_2 :: Eq a => [a] -> [[a]]
p09_2 = ifM null (const []) (   liftA2 (,,[]) id (:[]) . last >>=    (( liftA2  (:) (^._2) (^._3) . ) . ( .   init ) . foldr foldrHelper ) )
  where
    foldrHelper :: Eq a => a -> (a , [a], [[a]]) -> (a, [a], [[a]])
    foldrHelper = ap (flip ifM ifMTrueBranch .  ( . (^._1))  .  (==))   (( . liftA2 (:) (^._2) (^._3)) . liftA2 (,,) id (:[])   )
      where
        ifMTrueBranch :: (a,[a],[[a]]) -> (a,[a],[[a]])
        ifMTrueBranch = join (over _2 . (:) . (^._1))



--using until with takewhile and dropwhile
p09_3 :: Eq a => [a] -> [[a]]
p09_3 = reverse . snd . until ( null . fst ) nextStep . (,[])
  where
    nextStep :: Eq a => ([a],[[a]]) -> ([a],[[a]])
    nextStep = liftA2 (,) (join (dropWhile . (==) . head) . fst) ( (flip (:) . snd) <*>   join (takeWhile . (==) . head) . fst )

{-
p09_4 :: Eq a => [a] -> [[a]]
p09_4 =
-}
{-
--using fix
p09_2 :: Eq a => [a] -> [[a]];
p09_2 = fix (   ( . (flip if' []  . null) ) . if'  .   )
-}
--using fix tailrecursively
