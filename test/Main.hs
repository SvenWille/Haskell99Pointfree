{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree
import Data.Maybe
import Control.Exception
import Test.QuickCheck.Gen

main :: IO()

main =  hspec $ do
  p1Tests




p1Tests :: SpecWith ()
p1Tests = describe "Problem 1: return the last element of a list" $ do
  describe "testing version p1" $ do
    it "p1 with [1,2,3,4]" $
      p1 [1,2,3,4] `shouldBe` Just 4
    it "p1 with []" $
      p1 ([] :: [Int]) `shouldBe` Nothing
  describe "testing porperties of p1" $ do
    it "p1 with nonempty list" $ property 
      (\(x::[Int]) -> not  (null x) ==> (p1 x == (Just $ last x)))

  describe "testing version p1\'" $ do
    it "p1\' with [1,2,3,4]" $
      p1' [1,2,3,4] `shouldBe` 4
    it "p1\' with []" $
      evaluate (p1' []) `shouldThrow` anyException
  describe "testing properties of p1\'" $ do
    it "p1\' with nonempty list" $ property
      (\(x::[Bool]) -> not  (null x) ==> (p1' x == last x))

  describe "testing version p1\'\'" $ do
    it "p1\'\' with [1,2,3,4]" $
      p1' [1,2,3,4] `shouldBe` 4
    it "p1\'\' with []" $
      evaluate (p1' []) `shouldThrow` anyException
  describe "testing properties of p1\'\'" $ do
    it "p1\'' with nonempty list" $ property
      (\(x::String) -> not  (null x) ==> (p1' x == last x))
