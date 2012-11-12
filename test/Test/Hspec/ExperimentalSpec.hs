module Test.Hspec.ExperimentalSpec (main, spec) where

import           Test.Hspec.Meta
import qualified Test.Hspec.Experimental as H
import qualified Test.Hspec.Runner as H
import           System.IO.Silently
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

  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = fmap lines . capture_ . H.hspecWith H.defaultConfig
