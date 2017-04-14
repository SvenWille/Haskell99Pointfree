{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P95
    (p95, p95'
    ) where

import Data.List
import Data.Char
import Data.Bifunctor
import Control.Arrow
import Data.Bool.HT
import Control.Applicative
import Control.Monad
import Control.Monad.Extra

--crashes on negative numbers
p95 :: Int -> String
p95 = let nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"] in tail . concatMap ( (:) '-' . (!!) nums . digitToInt)  . show

-- takes the absolute value
p95' :: Int -> String
p95' =  intercalate "-" . map (  (!!) nums . digitToInt) . show . abs
  where
     nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"]

--using until
p95'' :: Int -> String
p95'' = intercalate "-" .  map  (nums !!) .   snd .  until ( (>) 1 . fst ) ( ( flip div 10 . fst)  &&&   uncurry( (:) .  (`mod` 10))  ) . join( (. (  (:[])  . flip mod 10)) . (,) . flip div 10)
  where
    nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"]


--safe version
p95''' :: Int -> Maybe String
p95''' =  liftA2  (`ifThenElse` Nothing)  (< 0) ( Just . intercalate "-" . map (nums !!) . snd .  until ( (< 1) . fst ) ( liftA2 (,) ( (`div` 10) . fst)  (uncurry( (:)  . (`mod` 10) )) )  . liftA2 (,) (`div` 10) ((:[]) . (`mod` 10)))
  where
    nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"]

--safe version that also works with negative numbers
p95_4 :: Int -> String
p95_4 = join (   ifM (< 0)  .   (("minus-" ++) . ) )  ( intercalate "-" . map (nums !!) . snd . join  (  ( . ) .  until  ((< 1) . fst)  )     (liftA2 (,) ( flip div 10 . fst )  (uncurry( (:) .  flip mod 10))) . (,[]) .abs )
  where
    nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"]
