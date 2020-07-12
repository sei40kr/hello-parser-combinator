module LibSpec
  ( spec
  )
where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           Lib

spec :: Spec
spec = do
  describe "anyChar" $ do
    it "test1" $ anyChar "abc" `shouldBe` ('a', "bc")
  describe "char" $ do
    it "test1" $ char 'a' "abc" `shouldBe` ('a', "bc")
  describe "digit" $ do
    it "test1" $ digit "123" `shouldBe` ('1', "23")
  describe "letter" $ do
    it "test1" $ letter "abc" `shouldBe` ('a', "bc")
