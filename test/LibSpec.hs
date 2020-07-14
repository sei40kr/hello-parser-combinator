module LibSpec
  ( spec
  )
where

import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           Lib

spec :: Spec
spec = do
  describe "anyChar" $ do
    it "test1" $ runStateT anyChar "abc" `shouldBe` Right ('a', "bc")
    it "test2" $ runStateT anyChar "" `shouldBe` Left "too short"
  describe "char" $ do
    it "test1" $ runStateT (char 'a') "abc" `shouldBe` Right ('a', "bc")
    it "test2" $ runStateT (char 'a') "123" `shouldBe` Left "not char 'a': '1'"
  describe "digit" $ do
    it "test1" $ runStateT digit "123" `shouldBe` Right ('1', "23")
    it "test2" $ runStateT digit "abc" `shouldBe` Left "not digit: 'a'"
  describe "letter" $ do
    it "test1" $ runStateT letter "abc" `shouldBe` Right ('a', "bc")
    it "test2" $ runStateT letter "123" `shouldBe` Left "not letter: '1'"
