{-# LANGUAGE ScopedTypeVariables #-}

module Tests.P2Tests
    ( p2Tests
    ) where

import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree
import Data.Maybe
import Control.Exception

p2Tests :: SpecWith ()
p2Tests = describe "Problem 2: return the penultimate element of a list" $ do
  describe "testing version p2" $ do
    it "p2 with [1,2,3,4]" $
      p2 [1,2,3,4] `shouldBe` 3
    it "p2 with []" $
      evaluate (p2 []) `shouldThrow` anyException
  describe "testing properties of p2" $
    it "p2 with nonempty list" $ property
      (\(x::[Int]) -> length x >= 2 ==> p2 x == last ( init x))

  describe "testing version p2\'" $ do
    it "p2\' with [1,2,3,4]" $
      p2' [1,2,3,4] `shouldBe` 3
    it "p2\' with []" $
      evaluate (p2' []) `shouldThrow` anyException
  describe "testing properties of p2'" $
    it "p2\' with nonempty list" $ property
      (\(x::String) -> length x >= 2 ==> p2' x == last ( init x))

  describe "testing version p2''" $ do
    it "p2'' with [1,2,3,4]" $
      p2'' [1,2,3,4] `shouldBe` Just 3
    it "p2'' with []" $
      p2'' ([]::[Int]) `shouldBe` Nothing
  describe "testing properties of p2''" $
    it "p2'' with nonempty list" $ property
      (\(x::String) -> length x >= 2 ==> p2'' x == Just (last $ init x))

  describe "testing version p2'''" $ do
    it "p2''' with [1,2,3,4]" $
      p2''' [1,2,3,4] `shouldBe` Just 3
    it "p2''' with []" $
      p2''' ([]::[Int]) `shouldBe` Nothing
  describe "testing properties of p2'''" $
    it "p2''' with nonempty list" $ property
      (\(x::[Integer]) -> length x >= 2 ==> p2''' x == Just (last $ init x))

  describe "testing version p2''''" $ do
    it "p2'''' with [1,2,3,4]" $
      p2'''' [1,2,3,4] `shouldBe` Just 3
    it "p2'''' with []" $
      p2'''' ([]::[Int]) `shouldBe` Nothing
  describe "testing properties of p2''''" $
    it "p2'''' with nonempty list" $ property
      (\(x::[Integer]) -> length x >= 2 ==> p2'''' x == Just (last $ init x))
