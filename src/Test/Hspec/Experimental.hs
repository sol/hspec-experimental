{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Experimental (
  module Test.Hspec
, it
) where

import           Test.Hspec hiding (it)
import           Test.Hspec.Core (Params(..), Result(..), SpecTree(..), fromSpecList)

import qualified Control.Exception as E
import           Test.HUnit.Lang (HUnitFailure(..))

import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck as QC

runExpectation :: Expectation -> QC.Property
runExpectation action = QCP.morallyDubiousIOProperty $ do
  (action >> return succeeded) `E.catch` \(HUnitFailure err) -> return (failed err)
  where
    succeeded  = QC.property QCP.succeeded
    failed err = QC.property QCP.failed {QCP.reason = err}

instance QC.Testable Expectation where
  property     = runExpectation
  exhaustive _ = True

it :: QC.Testable a => String -> a -> Spec
it s p = fromSpecList [SpecItem s result]
  where
    result c = do
      -- copied from Test.Hspec.Core.Type
      r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) p
      return $
        case r of
          QC.Success {}               -> Success
          f@(QC.Failure {})           -> Fail (QC.output f)
          QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ quantify n "test" )
          QC.NoExpectedFailure {}     -> Fail ("No expected failure")

quantify :: Int -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
