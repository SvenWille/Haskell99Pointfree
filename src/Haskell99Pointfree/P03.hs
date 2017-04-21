{-# LANGUAGE  TupleSections #-} --needed for p3'
module Haskell99Pointfree.P03
    (
    ) where

import Data.Bool.HT (if')
import Control.Monad
import Control.Lens
import Data.List (find) --used in p3_6
import Control.Monad.Fix (mfix,fix)
import Control.Monad.Extra (ifM)



--insecure
p03_1 :: [a] -> Int -> a
p03_1 =   flip ( (head . ) .  drop . subtract 1)

--safe version
p03_2 :: [a] -> Int -> Maybe a
p03_2 =   flip (  ( (^._3) . ) .  foldl  ( liftM3 if' (liftM2 (==) (^._1) (^._2) ) (  ( .  Just ) .   liftM2 (,,) ((+) (1::Int) . (^._1)) (^._2)   ) ( const . liftM3 (,,) ( (+) 1 . (^._1)) (^._2) (^._3) ) ) . (0,,Nothing :: Maybe a) . subtract 1)


--convoluted safe version, avoiding liftMn and liftAn
p03_3 :: [a] -> Int -> Maybe a
p03_3 = join .  join ( (    . (( flip if' Nothing .  ) . join . ( . (||) .(<= 0 )) . flip (.) . (<) .  length )  )  . (.) . flip (.) . ( (Just . last) .)  . flip take )


--"simplified" version of p03_2
p03_4 :: [a] -> Int -> Maybe a
p03_4 =   ( . subtract 1 ) .  ap ( liftM2 (flip ( `if'` Nothing))   (Just . head) .  ( . null) . (||) . (>) 0) . flip drop


--recursive
--not a very good solution since it introduces an extra parameter which must be used correctly (indexing starts with 1)
--I choose the third parameter as "counter"
{-
p03_5 :: [a] -> Int -> Int -> Maybe a
p03_5 = ap ( (flip if' Nothing . ) . ( . (<=1)) . (||) . null)  (  liftM2 (if')  )
-}

--using zip and find
p03_6 :: [a] -> Int -> Maybe a
p03_6 =  ( fmap snd . )   .   ( .  ( (. fst) .  (==) ))  . flip find   . zip [1..]


--using until
p03_7 :: [a] -> Int -> Maybe a
p03_7 =   join ( ( . alias1) . liftM2 ( `if'` Nothing) .  ( . (<= 0) ) . (||) . null )
  where
    alias1 = ( (  liftM2 ( `if'` Nothing)  null (Just . head)  . (^._3) . until  (liftM2 (||) (liftM2 (==) (^._1) (^._2)) (null . (^._3) ) )    (liftM3 (,,) ( (+1) . (^._1))  (^._2) (tail . (^._3))  )) . ) .  flip (1,,)
{-
--using  either with a default value
p03_8 :: [a] -> Int -> a -> Either a a
p03_8 =
-}

--using ap
p03_9 :: [a] -> Int -> Maybe a
p03_9 = ap (ap . ( flip if' Nothing . ) . liftM2 (||) (< 1)  . (<) . length )  ( ( (Just . head . (^._3) . until (liftM2 (==) (^._1) (^._2) ) (liftM3 (,,)  ((+1) . (^._1)) (^._2) (tail . (^._3)) )) . ) . flip (1,,))

--using mfix
p03_10 :: [a] -> Int -> Maybe a
p03_10 = ap (liftM2 (`if'` Nothing)  . liftM2 ( ( . ) . (||) ) null (liftM2 (||) (<= 0)  . (<) . length)) (flip (mfix ( ( . ( ( . fst) . (==))) .  (flip . flip (liftM3 if')) (Just . head . snd) . ( .  (over _2 tail . over _1 (+1) )))) . (1,))

--using foldr
p03_11 :: [a] -> Int -> Maybe a
p03_11 = ( .  (flip foldr Nothing.  join .   ((flip .flip  if') (const . Just. snd)  (const id) . )  . ( . fst)  . (==)   ) ) . flip ($) . zip [1..]

--using fix (not to be confused with mfix)
p03_12 :: [a] -> Int -> Maybe a
p03_12 = ap (flip if' Nothing . ((< 1):: Int -> Bool)) .  fix  ( flip .  liftM2 ( `if'` ( const . const Nothing)) null  .  branch) 1
  where
    branch :: (Int -> [a]  -> Int -> Maybe a) -> [a] -> Int -> Int -> Maybe a
    branch =  ap (   ap  .  ( . (==)) . liftM2  . flip if'  .  Just . head  )   . ( (. (+1)) . )  . ( . tail ) . flip
