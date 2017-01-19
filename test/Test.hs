-- |
-- Module      :  Main
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist

module Main where

-- Prelude
import Prelude.Unicode
import ClassyPrelude

-- Other modules
import           Test.Tasty
import qualified Test.DefinitionsTheoremsLemmas
import qualified Test.Equations
import qualified Test.Notes
import qualified Test.Premises
import qualified Test.Extract

main ∷ IO ()
main =
  defaultMain $ testGroup "Tests" [Test.DefinitionsTheoremsLemmas.tests
                                  , Test.Equations.tests
                                  , Test.Notes.tests
                                  , Test.Premises.tests
                                  , Test.Extract.tests
                                  ]
