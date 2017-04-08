module Haskell99Pointfree.P2 (p2,p2',p2'',p2''',p2'''',p2_5, p2_6 , p2_7) where


import Control.Applicative
import Control.Monad
import Data.Bool.HT (ifThenElse , if')
import Control.Monad.Extra (ifM)

--simple, unsafe version
p2 :: [a] -> a
p2 = head . tail . reverse  -- or just last . init


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


-- !!!!test missing!!!!!!!
--safe version (using either) and for convenience/variety ifM is used
p2_5 :: [a] -> Either String a
p2_5 = ifM ( (  >= 2 ) . length ) (Right . last . init) (const (Left "no such element"))

-- !!!!test missing!!!!!!!
--safe version (using a default value)
p2_6 :: [a] -> a -> a
p2_6 =    flip id ((  . ( last . init ) )  . flip  .  ifThenElse   . ( < 2) . length )    .  flip join

-- !!!!test missing!!!!!!!
--version with custom error message (using error)
p2_7 :: [a] -> a
p2_7 =  join ((   .  (last . init)  ) . flip id (error "no such element") .  ifThenElse  . ( < 2) . length)
