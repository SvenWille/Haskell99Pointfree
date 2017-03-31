module Haskell99Pointfree.P5
    ( p5,p5',p5'',p5''',p5_4,p5_5
    ) where


import Data.Bool.HT
import Control.Monad
import Control.Applicative
import Control.Monad.Extra


p5 :: [a] -> [a]
p5 =  ifM (not . null) (join    ((  . (flip (:) [] .  head )) . (++)  . p5 . tail) ) (const [])

--same as p5 but using ifThenElse instead of ifM
p5' :: [a] -> [a]
p5' = liftA3 ifThenElse null (const []) (  flip (flip (++) . take 1) =<< p5' . tail )

--variation of p5'
p5'' :: [a] -> [a]
p5''=  join   ((. ap (flip (++) . take 1) (p5'' . tail)) . flip if' [] . null  )

p5''' :: [a] -> [a]
p5'''  = snd . until (null . fst) (liftA2 (,) (tail . fst) (ap ( flip (:) . snd ) (head . fst))) . flip (,) []

--vairation of p5'''
p5_4 :: [a] -> [a]
p5_4 = snd . until (null . fst) (join ((. join ( ( . snd ) . (:) . head . fst) ) . (,) . tail  . fst )) . flip (,) []


p5_5 :: [a] -> [a]
p5_5 =   join (  (. (p5_5 . tail) ) . flip  ifThenElse [] . null ) >>=  flip (join (  ( . ( flip (++) . take 1 ) ) . flip ifThenElse (const []) . null ))
