--{-# LANGUAGE TemplateHaskell #-}
module Haskell99Pointfree.P15
    ( module Haskell99Pointfree.P15
    ) where


import Data.Generics




getField :: (Data r, Typeable v) => Int -> r -> Maybe  v
getField i r = gmapQi i ( cast ) r
