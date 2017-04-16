{-# LANGUAGE  TupleSections #-} --needed for p3'
module Haskell99Pointfree.P03
    (p03 , p03' , p03''', p03_6, p03_11
    ) where

import Data.Bool.HT (if')
import Control.Monad
import Control.Lens
import Data.List (find) --used in p3_6


--insecure
p03 :: [a] -> Int -> a
p03 =   flip ( (head . ) .  drop . subtract 1)

--safe version
p03' :: [a] -> Int -> Maybe a
p03' =   flip (  ( (^._3) . ) .  foldl  ( liftM3 if' (liftM2 (==) (^._1) (^._2) ) (  ( .  Just ) .   liftM2 (,,) ((+) (1::Int) . (^._1)) (^._2)   ) ( const . liftM3 (,,) ( (+) 1 . (^._1)) (^._2) (^._3) ) ) . (0,,Nothing :: Maybe a) . subtract 1)

{-
--convoluted safe version
p3'' :: [a] -> Int -> Maybe a
p3'' =  flip ( ( join ( ( (Just . head) ) . flip if' Nothing . null) .  ) .   ) . flip drop
-}

--"simplified" version of p3''
p03''' :: [a] -> Int -> Maybe a
p03''' =   ( . subtract 1 ) .  ap ( liftM2 (flip ( `if'` Nothing))   (Just . head) .  ( . null) . (||) . (>) 0) . flip drop

--using curry and uncurry
p03_4 :: [a] -> Int -> Maybe a
p03_4 = undefined

--recursive
{-
p3_5   :: [a] -> Int -> Maybe a
p3_5 =  flip if' Nothing . ( . (<= 0))  .  (||) .  null
-}

--using zip and find
p03_6 :: [a] -> Int -> Maybe a
p03_6 =  ( fmap snd . )   .   ( .  ( (. fst) .  (==) ))  . flip find   . zip [1..]

{-
--using until
p3_7 :: [a] -> Int -> Maybe a
p3_7 =
-}
--using uncurry

--using ap

--using fix

--using foldr
p03_11 :: [a] -> Int -> Maybe a
p03_11 = ( .  (flip foldr Nothing.  join .   ((flip .flip  if') (const . Just. snd)  (const id) . )  . ( . fst)  . (==)   ) ) . flip ($) . zip [1..]
