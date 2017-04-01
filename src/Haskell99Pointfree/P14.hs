module Haskell99Pointfree.P14
    ( p14
    ) where


p14, p14', p14'' :: [a] -> [a]


p14 = concatMap (replicate 2)


p14' = undefined -- foldr


p14'' = undefined 
