{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Experimental (
  module Test.Hspec
, it
) where

import           Test.Hspec hiding (it)
import           Test.Hspec.Core (Example(..), Params(..), SpecTree(..), fromSpecList)

import qualified Control.Exception as E
import           Test.HUnit.Lang (HUnitFailure(..))

import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck as QC (quickCheckWithResult)

runExpectation :: Expectation -> QC.Property
runExpectation action = QC.morallyDubiousIOProperty $ do
  (action >> return succeeded) `E.catch` \(HUnitFailure err) -> return (failed err)
  where
    succeeded  = QC.property QC.succeeded
    failed err = QC.property QC.failed {QC.reason = err}

instance QC.Testable Expectation where
  property     = runExpectation
  exhaustive _ = True

it :: QC.Testable a => String -> a -> Spec
it s e = fromSpecList [SpecItem s result]
  where
    result c = QC.quickCheckWithResult (paramsQuickCheckArgs c) e >>= evaluateExample c
