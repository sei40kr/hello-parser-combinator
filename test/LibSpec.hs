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
  describe "satisfy" $ do
    it "test1" $ satisfy (== 'a') "abc" `shouldBe` ('a', "bc")
    it "test1" $ satisfy isDigit "123" `shouldBe` ('1', "23")
