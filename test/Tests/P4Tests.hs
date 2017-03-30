{-# LANGUAGE ScopedTypeVariables #-}
module Tests.P4Tests
    ( p4Tests
    ) where


import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree
import Data.Maybe
import Control.Exception
import Data.List

p4Tests :: SpecWith ()
p4Tests = describe "Problem 4: return the length of a list" $ do
  describe "testing version p4" $ do
    it "p4 with [1,2,3,4]" $
      p4 [1,2,3,4] `shouldBe` 4
    it "p4 with []" $
      p4 [] `shouldBe` 0
  describe "testing properties of p4" $
    it "p4 with arbitrary lists" $ property
      (\(x::[Int]) ->  p4 x == genericLength x)


  describe "testing version p4'" $ do
    it "p4' with [1,2,3,4,5]" $
      p4' [1,2,3,4,5] `shouldBe` 5
    it "p4' with []" $
      p4' [] `shouldBe` 0
  describe "testing properties of p4'" $
    it "p4' with arbitrary lists" $ property
      (\(x::[Int]) ->  p4' x == genericLength x)

  describe "testing version p4'" $ do
    it "p4'' with [1,2,3,4,5,6]" $
      p4'' [1,2,3,4,5,6] `shouldBe` 6
    it "p4'' with []" $
      p4'' [] `shouldBe` 0
  describe "testing properties of p4''" $
    it "p4'' with arbitrary lists" $ property
      (\(x::[Int]) ->  p4'' x == genericLength x)

  describe "testing version p4'''" $ do
    it "p4''' with [1,2,3,4,5,6,7]" $
      p4''' [1,2,3,4,5,6,7] `shouldBe` 7
    it "p4''' with []" $
      p4''' [] `shouldBe` 0
  describe "testing properties of p4'''" $
    it "p4''' with arbitrary lists" $ property
      (\(x::[Int]) ->  p4''' x == genericLength x)

  describe "testing version p4''''" $ do
    it "p4'''' with [1,2,3,4,5,6,7,8]" $
      p4'''' [1,2,3,4,5,6,7,8] `shouldBe` 8
    it "p4'''' with []" $
      p4'''' [] `shouldBe` 0
  describe "testing properties of p4''''" $
    it "p4'''' with arbitrary lists" $ property
      (\(x::[Int]) ->  p4'''' x == genericLength x)

  describe "testing version p4_5" $ do
    it "p4_5 with [1,2,3,4,5,6,7,8,9]" $
      p4_5 [1,2,3,4,5,6,7,8,9] `shouldBe` 9
    it "p4_5 with []" $
      p4_5 [] `shouldBe` 0
  describe "testing properties of p4_5" $
    it "p4_5 with arbitrary lists" $ property
      (\(x::[Int]) ->  p4_5 x == genericLength x)
