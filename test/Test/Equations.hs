-- |
-- Module      :  Test.Equations
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.Equations module.

module Test.Equations where

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
import           Text.LaTeX.Base.Parser (parseLaTeX)
import           Text.LaTeX.Base.Syntax

-- modules we will test
import Extract.Equations
import Types

-- TODO: use name
makeEquationNoteText ∷ Text → Text → Text → Text → Text
makeEquationNoteText _ setup eq1 eq2 =
  makeNoteText (setup ++ makeEnvText "equation*" eq1)
               (setup ++ makeEnvText "equation*" eq2)

testMakeEquationNote ∷ TestTree
testMakeEquationNote = testGroup "makeEquationNote" $
  [ HU.testCase "empty note" $
      makeEquationNoteText "" "" "=" "" @=?
      showNote (makeEquationNote (TeXRaw "", TeXRaw "", ""))
  , HU.testCase "non-empty note" $
      makeEquationNoteText "" "" "a=" "a=b" @=?
      showNote (makeEquationNote (TeXRaw "", TeXRaw "", "a=b"))
  , HU.testCase "note with setup" $
      makeEquationNoteText "" "setup" "x=" "x=y" @=?
      showNote (makeEquationNote (TeXRaw "", TeXRaw "setup", "x=y"))
  ]

testEquations ∷ TestTree
testEquations = testGroup "equations" $
  let errs = fst ∘ equations -- collect just the errors
  in []
  -- [ HU.testCase "succeeds on empty string" $
  --     [] =|= errs (TeXRaw "")
  -- , HU.testCase "succeeds on \\equation with 1st, 2nd, 3rd arg" $
  --      =|= "\\equation{n}{s}{e}"
  -- ]

tests ∷ TestTree
tests = testGroup "Equations" [ testCheckEquationArgs
                              , testMakeEquationNote
                              , testEquations
                              ]
