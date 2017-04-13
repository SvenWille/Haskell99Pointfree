module Haskell99Pointfree.P1 (p01,p01',p01'',p01''',p01'''',p01_5) where


import Data.Bool.HT
import Control.Monad
import Control.Applicative
import Control.Monad.Extra


-- safe version using maybe
-- instead of "head . reverse" one could use "last"
p01 :: [a] -> Maybe a
p01 = flip (join ((( . Just. head . reverse) . if') . not . null)) Nothing

-- simple version, crashes on empty lists
p01' :: [a] -> a
p01' = head . reverse

--using the error function with a choosen error message instead
--of the error message given by the head function.
--one could use liftM3 instead of liftA3
p01'' :: [a] -> a
p01'' = liftA3 if' (not . null) (head . reverse) (error "no last element for empty lists")

--using either for exception handling
p01''' :: [a] -> Either String a
p01''' = flip flip (Left "no last element for empty lists")  (liftA2 ifThenElse (not . null) (Right . last))

-- using a default value
p01'''' :: [a] -> a -> a
p01'''' = liftA2 ifThenElse (not . null) last

--using ifM from Control.Monad.Extra
p01_5 :: [a] -> Maybe a
p01_5 = ifM null (const Nothing) (Just . last)
