-- |
-- Module      :  Test.Extract
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Test the Extract top-level module.
-- ???
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe #-}

-- "import safe" is enforced.
module Test.Extract where

import ClassyPrelude
import Test.Tasty
import qualified Test.Tasty.Golden as GT
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Test.Util

-- Other modules
import Control.Eff
import Control.Eff.Writer.Strict
import Text.LaTeX (renderFile)
import Text.LaTeX.Base.Parser (parseLaTeX)

-- modules we will test
import Extract
import Types

-- read an input file, write an output file
inAndOut :: FilePath -> FilePath -> IO ()
inAndOut inPath outPath = do
  content <- readFile inPath
  case (parseLaTeX content) of
    Right srcTeX ->
      let (_ :: [Log], (_ :: [Error], tex)) =
            run (runMonoidWriter (textToNotecards srcTeX))
      in renderFile outPath tex
    Left _ -> pure ()

testTextToNotecards :: TestTree
testTextToNotecards =
  testGroup "textToNotecards" $
  -- read as "the errors (the first component of the (second component of the)
  -- tuple) are empty"
  let noErrors x =
        let (_ :: [Log], (errs, _)) = run (runMonoidWriter (textToNotecards x))
        in ([] :: [Error]) =|= errs
  in [ HU.testCase "succeeds on empty string" $ noErrors ""
     , HU.testCase "does fine on one def" $
       noErrors "\\begin{definition}[test]testdef\\end{definition}"
  -- TODO
  -- , testGroup "Golden"
  --   [ GT.goldenVsFileDiff
  --       "GT: 1"
  --       (\gold out -> ["diff", gold, out])
  --       "test/golden/1.golden.tex"
  --       "test/golden/1.out.tex"
  --       (inAndOut "test/golden/1.in.tex" "test/golden/1.out.tex")
  --   ]
     ]

tests :: TestTree
tests = testGroup "Extract" [testTextToNotecards]
