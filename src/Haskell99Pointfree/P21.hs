module Haskell99Pointfree.P21
    (p21
    ) where



--simple solution
p21 :: [a] -> Int -> [a]
p21 = splitAt
