module Haskell99Pointfree.P1 (p01_1,p01_2,p01_3,p01_4,p01_5,p01_6) where


import Data.Bool.HT (if', ifThenElse)
import Control.Monad (join)
import Control.Applicative (liftA2, liftA3, (<*>))
import Control.Monad.Extra (ifM)
import Control.Monad.Fix (fix)


-- safe version using maybe
-- instead of "head . reverse" one could use "last"
p01_1 :: [a] -> Maybe a
p01_1 = flip (join ((( . Just. head . reverse) . if') . not . null)) Nothing

-- simple version, crashes on empty lists
p01_2 :: [a] -> a
p01_2 = head . reverse

--using the error function with a choosen error message instead
--of the error message given by the head function.
--one could use liftM3 instead of liftA3
p01_3 :: [a] -> a
p01_3 = liftA3 if' (not . null) (head . reverse) (error "no last element for empty lists")

--using either for exception handling
p01_4 :: [a] -> Either String a
p01_4 = flip flip (Left "no last element for empty lists")  (liftA2 ifThenElse (not . null) (Right . last))

-- using a default value
p01_5 :: [a] -> a -> a
p01_5 = liftA2 ifThenElse (not . null) last

--using ifM from Control.Monad.Extra
p01_6 :: [a] -> Maybe a
p01_6 = ifM null (const Nothing) (Just . last)

--using fix for recursion
p01_7 :: [a] -> Maybe a
p01_7 = fix (ifM null  (const Nothing) . ifM ( (== 1) . length) (Just . head) .  ( . tail) )

p01_8 :: [a] -> Maybe a
p01_8 = flip if' Nothing . null <*>  Just .last

p01_9 :: [a] -> Maybe a
p01_9 = Just . last >>= ( . null) . flip ( `if'` Nothing) 
