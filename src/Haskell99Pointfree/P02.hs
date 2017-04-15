module Haskell99Pointfree.P02 (p02,p02',p02'',p02''',p02'''',p02_5, p02_6 , p02_7, p02_8) where


import Control.Applicative
import Control.Monad
import Data.Bool.HT (ifThenElse , if')
import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)

--simple, unsafe version
p02 :: [a] -> a
p02 = head . tail . reverse  -- or just last . init


--more complex and convoluted version, still unsafe
--calculates the length of the list and drops N-1 elements
p02' :: [a] -> a
p02' =  head . ap (flip drop)   ( flip (-) 2 . length)

--safe version (using maybe)
p02'' :: [a] -> Maybe a
p02'' = join ( (. (Just . head . tail . reverse) ) .  ($ Nothing) .flip  . ifThenElse .  (2 <=) . length )

--alternative version of p2''
p02''' :: [a] -> Maybe a
p02''' = liftA3 ifThenElse ( (<=) 2  . length ) (Just . head . tail . reverse ) (const Nothing)


--another alternative version of p2'' which is using (.) instead of ($)
p02'''':: [a] -> Maybe a
p02'''' = join ((. (Just . head . tail . reverse)) . flip (. id) Nothing  . flip . ifThenElse .  (2 <=) . length )


-- !!!!test missing!!!!!!!
--safe version (using either) and for convenience/variety ifM is used
p02_5 :: [a] -> Either String a
p02_5 = ifM ( (  >= 2 ) . length ) (Right . last . init) (const (Left "no such element"))

-- !!!!test missing!!!!!!!
--safe version (using a default value)
p02_6 :: [a] -> a -> a
p02_6 =    flip id ((  . ( last . init ) )  . flip  .  ifThenElse   . ( < 2) . length )    .  flip join

-- !!!!test missing!!!!!!!
--version with custom error message (using error)
p02_7 :: [a] -> a
p02_7 =  join ((   .  (last . init)  ) . flip id (error "no such element") .  ifThenElse  . ( < 2) . length)

--using fix for recursion
p02_8 :: [a] -> Maybe a
p02_8 = fix (ifM ( (< 2) . length )  (const Nothing) . ifM ( (== 2) . length) (Just . head) .  ( . tail) )
