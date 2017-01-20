-- |
-- Module      :  Test.Premises
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.Premises module.
-- TODO: test with first class patterns?
module Test.Premises where

-- Testing modules
import Test.Tasty
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Test.Util

-- Other modules
import ClassyPrelude
import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax

-- modules we will test
import Extract.Premises
import Types

testHasPremises :: TestTree
testHasPremises =
  testGroup "hasPremises" $
  [ HU.testCase "sequence containing premises" $
    True @=? hasPremises (TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
  , HU.testCase "false on TeXEmpty" $ False @=? hasConclusion TeXEmpty
  ]

testHasConclusion :: TestTree
testHasConclusion =
  testGroup "hasConclusion" $
  [ HU.testCase "sequence containing conclusion" $
    True @=? hasConclusion (TeXSeq (TeXEnv "conclusion" [] TeXEmpty) TeXEmpty)
  , HU.testCase "false on TeXEmpty" $ False @=? hasConclusion TeXEmpty
  ]

testExtractPremises :: TestTree
testExtractPremises =
  testGroup "extractPremises" $
  let buildPremises content after =
        TeXSeq (TeXRaw "If") (TeXSeq (TeXEnv "premises" [] content) after)
  in [ QC.testProperty "QC: nothing for arbitrary latex blocks" $
       (== Nothing) . extractPremises
     , HU.testCase "TeXEmpty doesn't contain premises" $
       Nothing @=? extractPremises TeXEmpty
     , HU.testCase "behaves as expected on parsed LaTeX" $
       Right (Just TeXEmpty) @=?
       fmap
         extractPremises
         (parseLaTeX "If\\begin{premises}\\end{premises}then")
     ]

testExtractConclusion :: TestTree
testExtractConclusion =
  testGroup "extractConclusion" $
  [ QC.testProperty "QC: nothing for arbitrary latex blocks" $
    (== Nothing) . extractConclusion
  , HU.testCase "TeXEmpty doesn't contain conclusion" $
    Nothing @=? extractConclusion TeXEmpty
  , HU.testCase "behaves as expected on parsed LaTeX" $
    Right (Just TeXEmpty) @=?
    fmap
      extractPremises
      (parseLaTeX
         "If\\begin{premises}\\end{premises}then\\begin{conclusion}\\end{conclusion}")
  ]

testPremisesAndConclusion :: TestTree
testPremisesAndConclusion =
  testGroup "premisesAndConclusion" $
  [ QC.testProperty "QC: Nothing for arbitrary latex blocks" $
    (== Nothing) . premisesAndConclusion
  , HU.testCase "returns Nothing on TeXEmpty" $
    Nothing @=? premisesAndConclusion ([], TeXEmpty)
  , HU.testCase "returns Nothing on lemma with no Premises" $
    Nothing @=? premisesAndConclusion ([], TeXEnv "lemma" [] TeXEmpty)
  , HU.testCase "catches malformed premises" $
    Just (Left (UserError "Malformed premises/conclusion in (untitled)")) @=?
    premisesAndConclusion ([], TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
  , HU.testCase "catches unmatched premises" $
    let tex =
          TeXSeq (TeXRaw "") (TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
    in Just (Left (PremisesWithoutConclusion tex)) @=?
       premisesAndConclusion ([], tex)
  ]

testPremises :: TestTree
testPremises =
  testGroup "premises" $
  [ HU.testCase "succeeds on empty string" $
    ([] :: [Error]) =|= fst (premises (TeXRaw ""))
  , QC.testProperty "QC: empty on arbitrary latex blocks" $
    (== []) . snd . premises
  ]

tests :: TestTree
tests =
  testGroup
    "Premises"
    [ testHasPremises
    , testHasConclusion
    , testExtractPremises
    , testExtractConclusion
    , testPremisesAndConclusion
    , testPremises
    ]
