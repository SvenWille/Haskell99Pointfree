module Haskell99Pointfree.P32
    (p32_1, p32_2
    ) where


import Control.Lens
import Control.Applicative (liftA2)
import Data.Tuple.HT
import Control.Monad.Extra (ifM)
import Data.Bool.HT (if')
import Control.Monad.Fix (fix)
import Control.Monad (ap)

p32_1 :: Int -> Int -> Int
p32_1 =   ( (abs  . fst . until ( (==0 ) . snd) ( liftA2 (,) snd  (uncurry mod) )) . ) . (,)

--using a fixpoint
p32_2 :: Int -> Int -> Int
p32_2 = fix ( ap ( flip liftA2 (==0) .flip if' . abs)  .  (  .  mod )  . ap   )
