module Haskell99Pointfree.P95
    (p95, p95'
    ) where

import Data.List
import Data.Char
import Data.Bifunctor
import Control.Arrow


p95 :: Int -> String
p95 = let nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"] in tail . concatMap ( (:) '-' . (!!) nums . digitToInt)  . show


p95' :: Int -> String
p95' =  intercalate "-" . map (  (!!) nums . digitToInt) . show
  where
     nums = ["zero", "one" , "two", "three", "four", "five", "six", "seven", "eight", "nine"]

--using until
p95'' :: Int -> String
p95'' = undefined
--safe version
p95''' :: Int -> Maybe String
p95''' = undefined

--safe version that also works with negative numbers
p95_4 :: Int -> Maybe String
p95_4 = undefined
