{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P62
    (
    ) where



import Control.Monad.Fix (fix)
import Control.Applicative (liftA2, liftA3 , (<*>))
import Control.Monad.Extra (ifM)
import Control.Lens (makePrisms)
import Control.Lens.Extras (is)

data Tree a = Empty  | Branch {val :: a , lft :: Tree a, rgt :: Tree a} deriving (Show , Eq)

makePrisms ''Tree
--using fix
p62_1 :: Tree a -> [a]
p62_1 = fix ( ifM (is _Empty) (const []) . ifM (liftA2 (&&) (is _Empty . lft) (is _Empty. rgt)) (const []) .   (   liftA3  ((. (++)) . (.) . (:)) val  . ( . lft)  <*> ( . rgt)) )
