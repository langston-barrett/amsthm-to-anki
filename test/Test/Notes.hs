-- |
-- Module      :  Test.Notes
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.Notes module.
module Test.Notes where

-- Testing modules
import ClassyPrelude hiding ((<>))
import Test.Tasty
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import Test.Util

-- Other modules
import Text.LaTeX
import Text.LaTeX.Base.Syntax

-- modules we will test
import Extract.Notes
import Types

emptyField :: LaTeX
emptyField = TeXEnv "field" [] (TeXRaw "")

emptyNote :: LaTeX
emptyNote = TeXEnv "note" [] (emptyField <> emptyField)

testNoteID :: TestTree
testNoteID =
  testGroup "testNoteID" $
  let field text = TeXEnv "field" [] (TeXRaw text)
  in [ HU.testCase "empty note" $
       Right (makeNoteText "" "") @=? fmap showNote (noteID emptyNote)
     , HU.testCase "non-empty note" $
       Right (makeNoteText "field1" "field2") @=?
       fmap
         showNote
         (noteID (TeXEnv "note" [] (field "field1" <> field "field2")))
     ]

testNotes :: TestTree
testNotes =
  testGroup "notes" $
  -- read as "the errors (the first component of the tuple) are empty"
  let noErrors = (=|= []) . fst . notes
  in [ HU.testCase "succeeds on empty string" (noErrors (TeXRaw ""))
     , HU.testCase "succeeds on TeXEmpty" (noErrors TeXEmpty)
     , HU.testCase "fails on note without fields" $
       [UserError "Malformed note environment: no fields found"] @=?
       fst (notes (TeXEnv "note" [] mempty))
     , HU.testCase "succeeds on note with fields" (noErrors emptyNote)
     ]

tests :: TestTree
tests = testGroup "Notes" [testNoteID, testNotes]
