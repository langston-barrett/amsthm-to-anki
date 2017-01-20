-- |
-- Module      :  Test.Equations
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.Equations module.
module Test.Equations where

-- Testing modules
import Test.Tasty
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Test.Util

-- Other modules
import ClassyPrelude
import Text.LaTeX
import Text.LaTeX.Base.Parser (parseLaTeX)
import Text.LaTeX.Base.Syntax

-- modules we will test
import Extract.Equations
import Types

-- TODO: use name
makeEquationNoteText :: Text -> Text -> Text -> Text -> Text
makeEquationNoteText _ setup eq1 eq2 =
  makeNoteText
    (setup ++ makeEnvText "equation*" eq1)
    (setup ++ makeEnvText "equation*" eq2)

testDescendTeX :: TestTree
testDescendTeX =
  testGroup "descendTeX" $
  [ HU.testCase "gives back singleton list on singleton tex" $
    [TeXEmpty] @=? descendTeX id TeXEmpty
  , HU.testCase "counts tree properly" $
    4 @=? length (descendTeX id (TeXEnv "" [] (TeXSeq (TeXRaw "") (TeXRaw ""))))
  , testGroup "QuickCheck" $
    [ QC.testProperty "QC: result list is always non-empty" $
      (/= []) . descendTeX id
    , QC.testProperty "QC: first element of list is root of tree" $ \l ->
        Just l == headMay (descendTeX id l)
    ]
  ]

testSplitEqEnv :: TestTree
testSplitEqEnv =
  testGroup "splitEqEnv" $
  [ HU.testCase "fails on TeXEmpty" $
    (Left undefined :: Either Error (Maybe LaTeX, LaTeX, LaTeX)) =|=
    splitEqEnv ([], TeXEmpty)
  -- TODO
  ]

testMakeEquationNote :: TestTree
testMakeEquationNote =
  testGroup "makeEquationNote" $
  [ HU.testCase "empty note" $
    makeEquationNoteText "" "" "=" "" @=?
    showNote (makeEquationNote (Nothing, TeXRaw "", ""))
  , HU.testCase "non-empty note" $
    makeEquationNoteText "" "" "a=" "a=b" @=?
    showNote (makeEquationNote (Nothing, TeXRaw "", "a=b"))
  , HU.testCase "note with setup" $
    makeEquationNoteText "" "setup" "x=" "x=y" @=?
    showNote (makeEquationNote (Nothing, TeXRaw "setup", "x=y"))
  ]

testEquations :: TestTree
testEquations =
  testGroup "equations" $
  let errs = fst . equations -- collect just the errors
  in []
  -- [ HU.testCase "succeeds on empty string" $
  --     [] =|= errs (TeXRaw "")
  -- , HU.testCase "succeeds on \\equation with 1st, 2nd, 3rd arg" $
  --      =|= "\\equation{n}{s}{e}"
  -- ]

tests :: TestTree
tests =
  testGroup
    "Equations"
    [testDescendTeX, testSplitEqEnv, testMakeEquationNote, testEquations]
