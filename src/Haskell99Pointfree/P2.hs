module Haskell99Pointfree.P2 (p2,p2',p2'',p2''',p2'''',p2_5) where


import Control.Applicative
import Control.Monad
import Data.Bool.HT

--simple, unsafe version
p2 :: [a] -> a
p2 = head . tail . reverse


--more complex and convoluted version, still unsafe
--calculates the length of the list and drops N-1 elements
p2' :: [a] -> a
p2' =  head . ap (flip drop)   ( flip (-) 2 . length)

--safe version (using maybe)
p2'' :: [a] -> Maybe a
p2'' = join ( (. (Just . head . tail . reverse) ) .  ($ Nothing) .flip  . ifThenElse .  (2 <=) . length )

--alternative version of p2''
p2''' :: [a] -> Maybe a
p2''' = liftA3 ifThenElse ( (<=) 2  . length ) (Just . head . tail . reverse ) (const Nothing)


--another alternative version of p2'' which is using (.) instead of ($)
p2'''':: [a] -> Maybe a
p2'''' = join ((. (Just . head . tail . reverse)) . flip (. id) Nothing  . flip . ifThenElse .  (2 <=) . length )

--safe version (using either)
p2_5 :: [a] -> Either String a
p2_5 = undefined

--safe version (using a default value)
p2_6 :: [a] -> a -> a
p2_6 = undefined

--version with custom error message (using error)
