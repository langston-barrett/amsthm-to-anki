-- |
-- Module      :  Test.Premises
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.Premises module.

-- TODO: test with first class patterns?

module Test.Premises where

-- Prelude
import Prelude.Unicode
import ClassyPrelude

-- Testing modules
import           Test.Tasty
import           Test.Tasty.HUnit       ((@=?))
import qualified Test.Tasty.HUnit       as HU
import qualified Test.Tasty.QuickCheck  as QC
import           Test.Util
-- Other modules
import           Text.LaTeX
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.Base.Parser

-- modules we will test
import Extract.Premises
import Types

testDescendTeX ∷ TestTree
testDescendTeX = testGroup "descendTeX" $
  [ HU.testCase "gives back singleton list on singleton tex" $
      [TeXEmpty] @=? descendTeX id TeXEmpty
  , HU.testCase "counts tree properly" $
      4 @=? length (descendTeX id (TeXEnv "" [] (TeXSeq (TeXRaw "") (TeXRaw ""))))
  , testGroup "QuickCheck" $
    [ QC.testProperty "QC: result list is always non-empty" $
        (/= []) ∘ descendTeX id
    , QC.testProperty "QC: first element of list is root of tree" $
        \l → Just l == headMay (descendTeX id l)
    ]
  ]

testFilterTeX ∷ TestTree
testFilterTeX = testGroup "filterTeX" $
  [ QC.testProperty "QC: list is empty for constant False function" $
      (≡ []) ∘  filterTeX (\_ → False)
  , QC.testProperty "QC: list is full for constant True function" $
      \l → length (flattenTeX l) ≡ length (filterTeX (\_ → True) l)
  ]

testHasPremises ∷ TestTree
testHasPremises = testGroup "hasPremises" $
  [ HU.testCase "sequence containing premises" $
    True @=? hasPremises (TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
  , HU.testCase "false on TeXEmpty" $
      False @=? hasConclusion TeXEmpty
  ]

testHasConclusion ∷ TestTree
testHasConclusion = testGroup "hasConclusion" $
  [ HU.testCase "sequence containing conclusion" $
      True @=? hasConclusion (TeXSeq (TeXEnv "conclusion" [] TeXEmpty) TeXEmpty)
  , HU.testCase "false on TeXEmpty" $
      False @=? hasConclusion TeXEmpty
  ]

testExtractPremises ∷ TestTree
testExtractPremises = testGroup "extractPremises" $
  let buildPremises content after =
        TeXSeq (TeXRaw "If") (TeXSeq (TeXEnv "premises" [] content) after)
  in
  [ QC.testProperty "QC: nothing for arbitrary latex blocks" $
      (≡ Nothing) ∘ extractPremises
  , HU.testCase "TeXEmpty doesn't contain premises" $
      Nothing @=? extractPremises TeXEmpty
  , HU.testCase "behaves as expected on parsed LaTeX" $
      Right (Just TeXEmpty) @=?
      fmap extractPremises (parseLaTeX "If\\begin{premises}\\end{premises}then")
  ]

testExtractConclusion ∷ TestTree
testExtractConclusion = testGroup "extractConclusion" $
  [ QC.testProperty "QC: nothing for arbitrary latex blocks" $
      (≡ Nothing) ∘ extractConclusion
  , HU.testCase "TeXEmpty doesn't contain conclusion" $
      Nothing @=? extractConclusion TeXEmpty
  , HU.testCase "behaves as expected on parsed LaTeX" $
      Right (Just TeXEmpty) @=?
      fmap extractPremises (parseLaTeX "If\\begin{premises}\\end{premises}then\\begin{conclusion}\\end{conclusion}")
  ]

testPremisesAndConclusion ∷ TestTree
testPremisesAndConclusion = testGroup "premisesAndConclusion" $
  [ QC.testProperty "QC: Nothing for arbitrary latex blocks" $
      (≡ Nothing) ∘ premisesAndConclusion
  , HU.testCase "returns Nothing on TeXEmpty" $
      Nothing @=? premisesAndConclusion ([], TeXEmpty)
  , HU.testCase "returns Nothing on lemma with no Premises" $
      Nothing @=? premisesAndConclusion ([], TeXEnv "lemma" [] TeXEmpty)
  , HU.testCase "catches malformed premises" $
      Just (Left (UserError "Malformed premises/conclusion in (untitled)")) @=?
      premisesAndConclusion ([], TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
  , HU.testCase "catches unmatched premises" $
      let tex = TeXSeq (TeXRaw "") (TeXSeq (TeXEnv "premises" [] TeXEmpty) TeXEmpty)
      in Just (Left (PremisesWithoutConclusion tex)) @=? premisesAndConclusion ([], tex)
  ]

testPremises ∷ TestTree
testPremises = testGroup "premises" $
  [ HU.testCase "succeeds on empty string" $
      [] =|= fst (premises (TeXRaw ""))
  , QC.testProperty "QC: empty on arbitrary latex blocks" $
      (≡ []) ∘ snd ∘ premises
  ]

tests ∷ TestTree
tests = testGroup "Premises" [ testDescendTeX
                             , testFilterTeX
                             , testHasPremises
                             , testHasConclusion
                             , testExtractPremises
                             , testExtractConclusion
                             , testPremisesAndConclusion
                             , testPremises
                             ]
