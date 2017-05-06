module Haskell99Pointfree.P02 (p02_1,p02_2,p02_3,p02_4,p02_5,p02_6, p02_7 , p02_8, p02_9) where


import Control.Applicative (liftA2, liftA3, (<*>))
import Control.Monad (join, ap, (>=>))
import Data.Bool.HT (ifThenElse , if')
import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)
import Safe (initMay , lastMay)


--obviously none of the following solutions will work for infinite lists


--simple, unsafe version
p02_1 :: [a] -> a
p02_1 = head . tail . reverse  -- or just last . init


--more complex and convoluted version, still unsafe
--calculates the length of the list and drops N-1 elements
p02_2 :: [a] -> a
p02_2 =  head . ap (flip drop)   ( flip (-) 2 . length)

--safe version (using maybe)
p02_3 :: [a] -> Maybe a
p02_3 = join ( (. (Just . head . tail . reverse) ) .  ($ Nothing) .flip  . ifThenElse .  (2 <=) . length )

--using (<*>) from Control.Applicative
p02_3A :: [a] -> Maybe a
p02_3A =   flip if' Nothing . (< 2) . length <*>  Just . last . init

--alternative version of p02_3
p02_4 :: [a] -> Maybe a
p02_4 = liftA3 ifThenElse ( (<=) 2  . length ) (Just . head . tail . reverse ) (const Nothing)


--another alternative version of p2'' which is using (.) instead of ($)
p02_5:: [a] -> Maybe a
p02_5 = join ((. (Just . head . tail . reverse)) . flip (. id) Nothing  . flip . ifThenElse .  (2 <=) . length )


-- !!!!test missing!!!!!!!
--safe version (using either) and for convenience/variety ifM is used
p02_6 :: [a] -> Either String a
p02_6 = ifM ( (  >= 2 ) . length ) (Right . last . init) (const (Left "no such element"))

-- !!!!test missing!!!!!!!
--safe version (using a default value)
p02_7 :: [a] -> a -> a
p02_7 =    flip id ((  . ( last . init ) )  . flip  .  ifThenElse   . ( < 2) . length )    .  flip join

-- !!!!test missing!!!!!!!
--version with custom error message (using error)
p02_8 :: [a] -> a
p02_8 =  join ((   .  (last . init)  ) . flip id (error "no such element") .  ifThenElse  . ( < 2) . length)

--using fix for recursion
p02_9 :: [a] -> Maybe a
p02_9 = fix (ifM ( (< 2) . length )  (const Nothing) . ifM ( (== 2) . length) (Just . head) .  ( . tail) )

--using functions from the safe package
p02_10 :: [a] -> Maybe a
p02_10 = initMay >=> lastMay
