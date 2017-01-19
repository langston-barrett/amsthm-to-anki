-- |
-- Module      :  Main
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
module Main where

import ClassyPrelude
import qualified Test.DefinitionsTheoremsLemmas
import qualified Test.Equations
import qualified Test.Extract
import qualified Test.Notes
import qualified Test.Premises
import Test.Tasty

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ Test.DefinitionsTheoremsLemmas.tests
    , Test.Equations.tests
    , Test.Notes.tests
    , Test.Premises.tests
    , Test.Extract.tests
    ]
