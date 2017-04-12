{-# LANGUAGE TupleSections #-}
module Haskell99Pointfree.P23
    (p23'
    ) where

import System.Random
import Control.Applicative
import System.IO.Unsafe
import Control.Monad

--p23:: [a] -> Int -> IO [a]

--simplified version of p23 using unsafePerformIO (only useful for learning purposes)
p23' :: [a] -> Int ->  [a]
p23' =   ((reverse . snd ) . )   .  ap (  (.) . foldl  (uncurry(  ap (   (  . ) . (.) . (,) )    (  (  .  flip (:) ) . flip (.)  .   (!!)  )  )  )   . (,[])   )   ( (   . take  ) . flip id .   flip randomRs (unsafePerformIO getStdGen) . (0,) . subtract 1 . length )

{-

p23 :: []
-}
