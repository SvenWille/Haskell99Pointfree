{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P09
    (
    ) where


import Data.List
import Control.Lens
import Data.Bool.HT
import Control.Applicative
import Control.Monad

--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group


p09_2 :: Eq a => [a] -> [[a]]
p09_2 = join ( ( .  (((,[],[]) . head)  >>= (foldr  (join (   (   . ( ( . liftA2 (:) (^._2) (^._3) )  . liftA2 (,,) id (:[]))))))))   . (.) .  ( flip (liftA3 if') (liftA3 (,,) (^._1) (join( ( . (^._2))  . (:) .(^._1) )) (^._3) )    . (==) )        .  flip if' (const []) . null)


--using ap
