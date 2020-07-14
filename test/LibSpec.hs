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
    it "test1" $ runState anyChar "abc" `shouldBe` ('a', "bc")
  describe "char" $ do
    it "test1" $ runState (char 'a') "abc" `shouldBe` ('a', "bc")
  describe "digit" $ do
    it "test1" $ runState digit "123" `shouldBe` ('1', "23")
  describe "letter" $ do
    it "test1" $ runState letter "abc" `shouldBe` ('a', "bc")
