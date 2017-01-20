-- |
-- Module      :  Test.DefinitionsTheoremsLemmas
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract.DefinitionsTheoremsLemmas module.
module Test.DefinitionsTheoremsLemmas where

-- Testing modules
import ClassyPrelude
import Test.Tasty
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import Test.Util

-- Other modules
import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax

-- modules we will test
import Extract.DefinitionsTheoremsLemmas
import Extract.Util
import Types

-- TODO: find a home for these tests
testLookForEnv :: TestTree
testLookForEnv =
  testGroup "lookForEnvStar" $
  let fromStr :: String -> Text -> Either ParseError [([TeXArg], LaTeX)]
      fromStr str ltx = (lookForEnvStar str) <$> parseLaTeX ltx
  in [ HU.testCase "empty list on empty string" $
       Right [] @=? fromStr "environment-name" ""
     , HU.testCase "captures contents of theorem env" $
       Right [([], TeXRaw "thm")] @=?
       fromStr "theorem" "\\begin{theorem}thm\\end{theorem}"
     , HU.testCase "captures contents of lemma* env" $
       Right [([], TeXRaw "content")] @=?
       fromStr "lemma" "\\begin{lemma*}content\\end{lemma*}"
     ]

testMakeEnvNote :: TestTree
testMakeEnvNote =
  testGroup "makeEnvNote" $
  let errorsOn tex = assertLeft (makeEnvNote (makeAuxNote "" "") tex)
  in [ HU.testCase "fails on SymArg" $ errorsOn ([SymArg (TeXRaw "")], mempty)
     , HU.testCase "empty def" $
       Right (makeAuxNote "" "" (TeXRaw "test") mempty) @=?
       makeEnvNote (makeAuxNote "" "") ([OptArg (TeXRaw "test")], mempty)
     ]

testDefinitionsTheoremsLemmas :: TestTree
testDefinitionsTheoremsLemmas =
  testGroup "definitionsTheoremsLemmas" $
  [ HU.testCase "succeeds on empty string" $
    ([] :: [Error]) =|= fst (definitionsTheoremsLemmas (TeXRaw ""))
  -- TODO
  -- , HU.testCase "fails on definition w/o name" $
  --   assertRight (definitionsTheoremsLemmas (TeXRaw ""))
  ]

tests :: TestTree
tests =
  testGroup "" [testLookForEnv, testMakeEnvNote, testDefinitionsTheoremsLemmas]
