{-# LANGUAGE ScopedTypeVariables #-}
module Tests.P1Tests
    ( p1Tests
    ) where

import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree.P1
import Data.Maybe
import Control.Exception
import Data.Either

p1Tests :: SpecWith ()
p1Tests = describe "Problem 1: return the last element of a list" $ do
  describe "testing version p1" $ do
    it "p1 with [1,2,3,4]" $
      p1 [1,2,3,4] `shouldBe` Just 4
    it "p1 with []" $
      p1 ([] :: [Int]) `shouldBe` Nothing
  describe "testing porperties of p1" $
    it "p1 with nonempty list" $ property
      (\(x::[Int]) -> not  (null x) ==> (p1 x == (Just $ last x)))

  describe "testing version p1\'" $ do
    it "p1\' with [1,2,3,4]" $
      p1' [1,2,3,4] `shouldBe` 4
    it "p1\' with []" $
      evaluate (p1' []) `shouldThrow` anyException
  describe "testing properties of p1\'" $
    it "p1\' with nonempty list" $ property
      (\(x::[Bool]) -> not  (null x) ==> (p1' x == last x))

  describe "testing version p1\'\'" $ do
    it "p1\'\' with [1,2,3,4]" $
      p1'' [1,2,3,4] `shouldBe` 4
    it "p1\'\' with []" $
      evaluate (p1'' []) `shouldThrow` anyException
  describe "testing properties of p1\'\'" $
    it "p1\'' with nonempty list" $ property
      (\(x::String) -> not  (null x) ==> (p1'' x == last x))

  describe "testing version p1'''" $ do
    it "p1''' with [1,2,3,4]" $
      p1''' [1,2,3,4] `shouldBe` Right 4
    it "p1''' with []" $
      p1''' ([] :: [Int]) `shouldBe` Left "no last element for empty lists"
  describe "testing properties of p1'''" $
    it "p1''' with arbitrary lists" $ property
      (\(x::String) -> (null x && isLeft (p1''' [])  ) ||   (p1''' x == Right  (last x)))

  describe "testing version p1''''" $ do
    it "p1'''' with [1,2,3,4] 5" $
      p1'''' [1,2,3,4] 5 `shouldBe` 4
    it "p1'''' with [] 5" $
      p1'''' [] 5 `shouldBe` 5
  describe "testing properties of p1''''" $
    it "p1'''' with arbitrary lists" $ property
      (\(x::[Int]) (y::Int) -> if null x then p1'''' x y == y else p1'''' x y == last x )

  describe "testing version p1_5" $ do
    it "p1_5 with [1,2,3,4]" $
      p1_5 [1,2,3,4] `shouldBe` Just 4
    it "p1_5 with []" $
      p1_5 ([] :: [Int]) `shouldBe` Nothing
  describe "testing porperties of p1_5" $
    it "p1_5 with nonempty list" $ property
      (\(x::[Int]) -> (null x && isNothing (p1_5 x) ) || (p1_5 x == Just (last x)))
