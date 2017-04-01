module Haskell99Pointfree.P1 (p1,p1',p1'',p1''',p1'''',p1_5) where


import Data.Bool.HT
import Control.Monad
import Control.Applicative
import Control.Monad.Extra


-- safe version using maybe
-- instead of "head . reverse" one could use "last"
p1 :: [a] -> Maybe a
p1 = flip (join ((( . Just. head . reverse) . if') . not . null)) Nothing

-- simple version, crashes on empty lists
p1' :: [a] -> a
p1' = head . reverse

--using the error function with a choosen error message instead
--of the error message given by the head function.
--one could use liftM3 instead of liftA3
p1'' :: [a] -> a
p1'' = liftA3 if' (not . null) (head . reverse) (error "no last element for empty lists")

--using either for exception handling
p1''' :: [a] -> Either String a
p1''' = flip flip (Left "no last element for empty lists")  (liftA2 ifThenElse (not . null) (Right . last))

-- using a default value
p1'''' :: [a] -> a -> a
p1'''' = liftA2 ifThenElse (not . null) last

--using ifM from Control.Monad.Extra
p1_5 :: [a] -> Maybe a
p1_5 = ifM null (const Nothing) (Just . last)
