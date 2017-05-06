module Haskell99Pointfree.P05
    ( p05_1,p05_2,p05_3,p05_4,p05_5,p05_6
    ) where


import Data.Bool.HT
import Control.Monad
import Control.Applicative
import Control.Monad.Extra (ifM)
import Data.List.Extra (snoc, cons)
import Control.Monad.Fix (fix)


p05_1 :: [a] -> [a]
p05_1 =  ifM (not . null) (join    ((  . (flip (:) [] .  head )) . (++)  . p05_1 . tail) ) (const [])


p05_2 :: [a] -> [a]
p05_2 = liftA3 ifThenElse null (const []) (  flip (flip (++) . take 1) =<< p05_2 . tail )

--variation of p-5_2
p05_3 :: [a] -> [a]
p05_3=  join   ((. ap (flip (++) . take 1) (p05_3 . tail)) . flip if' [] . null  )

p05_4 :: [a] -> [a]
p05_4  = snd . until (null . fst) (liftA2 (,) (tail . fst) (ap ( flip (:) . snd ) (head . fst))) . flip (,) []

--vairation of p05_4
p05_5 :: [a] -> [a]
p05_5 = snd . until (null . fst) (join ((. join ( ( . snd ) . (:) . head . fst) ) . (,) . tail  . fst )) . flip (,) []


p05_6 :: [a] -> [a]
p05_6 =   join (  (. (p05_6 . tail) ) . flip  ifThenElse [] . null ) >>=  flip (join (  ( . ( flip (++) . take 1 ) ) . flip ifThenElse (const []) . null ))


p05_7 :: [a] -> [a]
p05_7 = foldr (flip snoc) []


p05_8 :: [a] -> [a]
p05_8 = foldl (flip (:)) []

{-
p05_9 :: [a] -> [a]
p05_9 = fix ()
-}
