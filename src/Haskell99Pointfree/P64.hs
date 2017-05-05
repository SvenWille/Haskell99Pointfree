module Haskell99Pointfree.P64
    (
    ) where


import Control.Monad.Fix (fix)
import Control.Applicative (liftA2, liftA3 , (<*>))
import Control.Monad.Extra (ifM)
import Control.Lens (makePrisms)
import Control.Lens.Extras (is)
import Control.Monad ((>>=))

type Pos = (Int,Int)



data Tree a = Empty  | Branch {val :: a , lft :: Tree a, rgt :: Tree a} deriving (Show , Eq)

makePrisms ''Tree
{-
p64_1 :: Tree a -> Tree (a,Pos)
p64_1 = fix   >>=
-}
