-- |
-- Module      :  Extract.DefinitionsTheoremsLemmas
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Extract definitions/theorems/lemmas, create appropriate notecards as
-- described in the README.

module Extract.DefinitionsTheoremsLemmas where

-- Prelude
import ClassyPrelude hiding ((<>))
import Prelude.Unicode

-- Imported modules
import Text.LaTeX
import Text.LaTeX.Base.Syntax

-- My modules
import Types
import Extract.Util

-- | This function exists purely to eliminate boilerplate in the following three.
-- It wraps the "front" of a card with s1 and s2 and leaves the "back" as is.
makeAuxNote ∷ Text → Text → LaTeX → LaTeX → Notecard
makeAuxNote s1 s2 f b =
  Notecard { front = (TeXRaw s1 <> f <> TeXRaw s2), back = b }

-- | Transforms a block representing a "definition/lemma/theorem" environment
-- into one representing a "note" environment with two "field"s.
--
-- Takes `makeDefinitionNote`, `makeTheoremNote`, or `makeLemmaNote` as a first
-- argument.
makeEnvNote ∷ (LaTeX → LaTeX → Notecard) → ([TeXArg], LaTeX) → Either Error Notecard
makeEnvNote makeSpecificNote (args, content) =
  case args of
    [OptArg term] → Right $ makeSpecificNote term content
    _ → Left (UserError $ "couldn't find title of this environment:\n" ++ render content)

definitionsTheoremsLemmas ∷ LaTeX → ([Error], [Notecard])
definitionsTheoremsLemmas srcTeX =
  partitionEithers $ map (makeEnvNote makeDefinitionNote)
                         (lookForEnvStar "definition" srcTeX) ++
                     map (makeEnvNote makeTheoremNote)
                         (lookForEnvStar "theorem" srcTeX) ++
                     map (makeEnvNote makeLemmaNote)
                         (lookForEnvStar "lemma" srcTeX)
  where
    makeDefinitionNote = makeAuxNote "Define the term " "."
    makeTheoremNote = makeAuxNote "What is the " " theorem?"
    makeLemmaNote = makeAuxNote "What is the " " lemma?"
