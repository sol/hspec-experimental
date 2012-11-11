module Main (main, spec) where

import Test.Hspec.Experimental
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" $
      \xs -> (reverse . reverse) xs `shouldBe` (xs :: [Int])
