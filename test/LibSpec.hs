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
    it "test2" $ runStateT anyChar "" `shouldBe` Left ("too short", "")
  describe "char" $ do
    it "test1" $ runStateT (char 'a') "abc" `shouldBe` Right ('a', "bc")
    it "test2" $ runStateT (char 'a') "123" `shouldBe` Left
      ("not char 'a': '1'", "123")
  describe "digit" $ do
    it "test1" $ runStateT digit "123" `shouldBe` Right ('1', "23")
    it "test2" $ runStateT digit "abc" `shouldBe` Left ("not digit: 'a'", "abc")
  describe "letter" $ do
    it "test1" $ runStateT letter "abc" `shouldBe` Right ('a', "bc")
    it "test2" $ runStateT letter "123" `shouldBe` Left
      ("not letter: '1'", "123")
  describe "many" $ do
    it "test1" $ evalStateT (many letter) "abc123" `shouldBe` Right "abc"
    it "test2" $ evalStateT (many letter) "123abc" `shouldBe` Right ""
    it "test3" $ evalStateT (many (letter <|> digit)) "abc123" `shouldBe` Right
      "abc123"
    it "test4" $ evalStateT (many (letter <|> digit)) "123abc" `shouldBe` Right
      "123abc"
  describe "many1" $ do
    it "test1" $ evalStateT (many1 $ char 'a') "" `shouldBe` Left
      ("not char 'a'too short", "")
    it "test1" $ evalStateT (many1 $ char 'a') "aaa" `shouldBe` Right "aaa"
  describe "string" $ do
    it "test1" $ evalStateT (string "ab") "ab" `shouldBe` Right "ab"
  describe "try" $ do
    it "test1"
      $          evalStateT
                   (try (sequence [char 'a', char 'b']) <|> sequence [char 'a', char 'c']
                   )
                   "ab"
      `shouldBe` Right "ab"
    it "test2"
      $          evalStateT
                   (try (sequence [char 'a', char 'b']) <|> sequence [char 'a', char 'c']
                   )
                   "ac"
      `shouldBe` Right "ac"
