module Haskell99Pointfree.P27
    (
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad.Extra (ifM)

p27_1 :: [Int] -> [a] -> [[a]]
p27_1 = ifM null ((( view _3 . until (liftA2 (null . view _3) (null . view _4))  nextStep)  . ) . ( . pure) . liftA2 ( , , ,[[]],[]) tail (pure . pure . head) ) (const . const [])
  where
    nextStep =
