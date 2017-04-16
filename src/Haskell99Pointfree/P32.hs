module Haskell99Pointfree.P32
    (
    ) where


import Control.Lens
import Control.Applicative

p32 :: Int -> Int -> Int
p32 = until (liftA2 (||) () () ) ()  . 
