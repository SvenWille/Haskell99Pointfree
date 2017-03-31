module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree
import Data.Maybe
import Control.Exception
import Test.QuickCheck.Gen
import Tests.P1Tests
import Tests.P2Tests
import Tests.P4Tests
import Tests.P5Tests

main :: IO()

main =  hspec $ do
  p1Tests
  p2Tests
  p4Tests
  p5Tests
