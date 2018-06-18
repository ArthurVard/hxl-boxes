module BoxesSpec where

import Boxes

import Test.Hspec
import Test.QuickCheck.Property


spec :: Spec
spec = do
  describe "elemAt" $ do
    it "returns the 0 index  element as a list" $ do
      elemAt 0 [10 ..]  `shouldBe` ([10] :: [Int])

    it "returns the 3 index  element as a list" $ do
      elemAt 3 [0 ..] `shouldBe` ([3] :: [Int])

    it "Negative index test" $ do
      elemAt (-4) [0 ..] `shouldBe` []

    it "Out of range index test" $ do
      elemAt 16 [0 .. 10] `shouldBe` ([] :: [Int])


    it "property test of getting 0 index element" $
      property $ \x xs -> elemAt 0 (x:xs) == ([x] :: [Int])

  describe "calculateJumps" $ do
    it "no way data sample [3,-1,6,-2,15,8]" $ do
      calculateJumps [3,-1,6,-2,15,8] `shouldBe` Nothing
    it " 2 jump before go out [3,-1,6,-22,15,8]" $ do
      calculateJumps [3,-1,6,-22,15,8] `shouldBe` (Just 2)
    it " 3 jump before go out [2,-1,3, 22,15,8]" $ do
      calculateJumps [2,-1,3, 22,15,8] `shouldBe` (Just 3)
    it "no way data [1,2,10,0]" $ do
      calculateJumps [1,2,10,0] `shouldBe` Nothing
