
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.P5Tests
    ( p5Tests
    ) where


import Test.Hspec
import Test.QuickCheck
import Haskell99Pointfree.P5
import Data.Maybe
import Control.Exception
import Data.List

p5Tests :: SpecWith ()
p5Tests = describe "Problem 5: reverse a list" $ do
  describe "testing version p5" $ do
    it "p5 with [1,2,3,4]" $
      p5 [1,2,3,4] `shouldBe` [4,3,2,1]
    it "p5 with []" $
      p5 ([]::[Int]) `shouldBe` []
  describe "testing properties of p5" $
    it "p5 with arbitrary lists" $ property
      (\(x::[Int]) ->  p5 x == reverse x)


  describe "testing version p5'" $ do
    it "p5' with [1,2,3,4,5]" $
      p5' [1,2,3,4,5] `shouldBe` [5,4,3,2,1]
    it "p5' with []" $
      p5' ([]::[Int]) `shouldBe` []
  describe "testing properties of p5'" $
    it "p5' with arbitrary lists" $ property
      (\(x::[Int]) ->  p5' x == reverse x)

  describe "testing version p5''" $ do
    it "p5'' with [1,2,3,4,5,6]" $
      p5'' [1,2,3,4,5,6] `shouldBe` [6,5,4,3,2,1]
    it "p5'' with []" $
      p5'' ([]::[Int]) `shouldBe` []
  describe "testing properties of p5''" $
    it "p5'' with arbitrary lists" $ property
      (\(x::[Int]) ->  p5'' x == reverse x)

  describe "testing version p5'''" $ do
    it "p5''' with [1,2,3,4,5,6,7]" $
      p5''' [1,2,3,4,5,6,7] `shouldBe` [7,6,5,4,3,2,1]
    it "p5''' with []" $
      p5''' ([]::[Int]) `shouldBe` []
  describe "testing properties of p5'''" $
    it "p5''' with arbitrary lists" $ property
      (\(x::[Int]) ->  p5''' x == reverse x)

  describe "testing version p5_4" $ do
    it "p5_4 with [1,2,3,4,5,6,7,8]" $
      p5_4 [1,2,3,4,5,6,7,8] `shouldBe` [8,7,6,5,4,3,2,1]
    it "p5_4 with []" $
      p5_4 ([]::[Int]) `shouldBe` []
  describe "testing properties of p5_4" $
    it "p5_4 with arbitrary lists" $ property
      (\(x::[Int]) ->  p5_4 x == reverse x)

  describe "testing version p5_5" $ do
    it "p5_5 with [1,2,3,4,5,6,7,8,9]" $
      p5_5 [1,2,3,4,5,6,7,8,9] `shouldBe` [9,8,7,6,5,4,3,2,1]
    it "p5_5 with []" $
      p5_5 [] `shouldBe` ([]::[Int])
  describe "testing properties of p5_5" $
    it "p5_5 with arbitrary lists" $ property
      (\(x::[Int]) ->  p5_5 x == reverse x)
