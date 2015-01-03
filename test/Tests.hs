{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Monad.Reader


testMushrooms :: LabeledMushrooms
testMushrooms = [(True, [1,2,3])]


spec :: Spec
spec = do
  describe "MyTEST" $ do
    it "a = a" $
      1 `shouldBe` 1
      

main = hspec spec
--return testGetItemDetailsSmall
