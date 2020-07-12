module LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck

import           Lib

spec :: Spec
spec = do
  describe "anyChar" $ do
    it "test1" $ anyChar "abc" `shouldBe` ('a', "bc")
