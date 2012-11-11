module Test.Hspec.ExperimentalSpec (main, spec) where

import           Test.Hspec.Meta
import qualified Test.Hspec.Experimental as H
import           Data.IORef

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expectation as an example" $ do
    it "is executed only once" $ do
      ref <- newIORef (23 :: Int)
      let testSpec = H.it "some behavior" (modifyIORef ref succ)
      H.hspec testSpec
      readIORef ref `shouldReturn` 24

    it "can be quantified" $ do
      ref <- newIORef (23 :: Int)
      let testSpec =
            H.it "some behavior" $ \xs -> do
              modifyIORef ref succ
              (reverse . reverse) xs `shouldBe` (xs :: [Int])
      H.hspec testSpec
      readIORef ref `shouldReturn` 123
