{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module  Haskell99Pointfree.P54
    (
    ) where


import Data.Data
import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving ( Typeable)


--trivial solution (let the type checker do the work)
--it's somewhat non sensical to implement a seperate function for this ...
p54_1 :: Tree a -> Bool
p54_1 = const True

--using data and typeable
--this solution is not completely right since the result depends on the type parameter
--of "Tree". So this solutions only returns true if the tree has the type "Tree Int"
p54_2 :: Typeable a =>  a -> Bool
p54_2 = (typeOf (undefined :: Tree Int) ==) . typeOf



--doing it better than in p54_2 but still not completely satifying
--this solution, unlike p54_2 ignores the type parameter of "Tree" but requires
--data types of kind * -> *
p54_3 :: (Typeable t) => t a -> Bool
p54_3  = (typeOf1 (undefined :: Tree () ) ==)  . typeOf1


{-
p54_4 :: Typeable a => a -> Bool
p54_4 =
-}
