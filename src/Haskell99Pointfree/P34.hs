module Haskell99Pointfree.P34
    (
    ) where

import Control.Applicative

--simple, ineffective solution for large numbers
p34_1 :: Int -> Int
p34_1 = ( length  .)    . filter . ( (== 1) . )  . gcd    <*>  enumFromTo 1 . subtract 1
