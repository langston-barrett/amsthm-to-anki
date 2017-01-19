-- |
-- Module      :  Extract.Premises
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Extract theorems and lemmas with premises, create appropriate notecards as
-- described in the README.
--
-- This file contains some unused functions that were utilized in previous
-- designs and seemed useful enough to keep.

module Extract.Premises where

-- Prelude
import ClassyPrelude hiding ((<>))
import Prelude.Unicode

-- Imported modules
import Text.LaTeX
import Text.LaTeX.Base.Syntax
-- import Text.LaTeX.Base.Render

-- My modules
import Extract.Util
import Types

-- | Descend a LaTeX document tree, applying the function at every level down to
-- the leaves, collecting the results. Only applies recursively in the case of
-- TeXEnv, TeXSeq, and TeXBraces, which are the two constructors that contain sub-latex.
descendTeX ∷ ∀ a. (LaTeX → a) → LaTeX → [a]
descendTeX f tex =
  let helper g tex =
        case tex of
          TeXBraces child → g tex : helper g child
          TeXEnv _ _ child → g tex : helper g child
          TeXMath _ child → g tex : helper g child
          TeXSeq child1 child2 → (g tex : helper g child1) ++ (helper g child2)
          _ → [g tex]
  in helper f tex

-- | Make a list of all of the levels of the LaTeX tree.
flattenTeX ∷ LaTeX → [LaTeX]
flattenTeX = descendTeX id

-- | Extract certain levels from the LaTeX tree.
filterTeX ∷ ∀ a. (LaTeX → Bool) → LaTeX → [LaTeX]
filterTeX f = filter f ∘ flattenTeX

-- | Is there a sub-block of this LaTeX block that contains a "premises"
-- environment?
hasPremises ∷ LaTeX → Bool
hasPremises = (>0) ∘ length ∘ lookForEnv "premises"

-- | Is there a sub-block of this LaTeX block that contains a "conclusion"
-- environment?
hasConclusion ∷ LaTeX → Bool
hasConclusion = (>0) ∘ length ∘ lookForEnv "conclusion"

-- | Extract the premises of a "theorem", "lemma", or "corrolary". To be used on
-- the content found by lookForEnvStar. Returns everything in the
-- \begin{premises} ... \end{premises} block.
extractPremises ∷ LaTeX → Maybe LaTeX
extractPremises tex =
  case tex of
    --       "If"             "premises"   "conclusion…"
    TeXSeq (TeXRaw _) (TeXSeq (TeXEnv "premises" [] premises) _) →
      Just premises
    _ → Nothing

-- | Extract the conclusion of a "theorem", "lemma", or "corrolary". To be used
-- on the content found by lookForEnvStar. Returns everything in the
-- \begin{conclusion} ... \end{conclusion} block.
extractConclusion ∷ LaTeX → Maybe LaTeX
extractConclusion tex =
  case tex of
    TeXSeq (TeXRaw _) -- "If"
            (TeXSeq
            (TeXEnv "premises" [] _) -- "premises"
            (TeXSeq
              (TeXRaw _) -- "then"
              (TeXSeq
                (TeXEnv "conclusion" [] conclusion)
                _ -- TeXRaw "\n", etc.
              ))) → Just conclusion
    _ → Nothing

-- | Takes the output of lookForEnv and creates a notecard using extractPremises
-- and extractConclusion. Warns on various cases of user error.
premisesAndConclusion ∷ ([TeXArg], LaTeX) → Maybe (Either Error Notecard)
premisesAndConclusion (args, tex) =
  let title =
        case args of
          [OptArg name] → name
          [] → "(untitled)"
  in
    case (extractPremises tex, extractConclusion tex) of
      (Just premises, Just conclusion) →
        Just $ Right $ Notecard
          { front =
              (TeXRaw "The premises of " <> title <> TeXRaw " are ") <>
              TeXEnv "itemize" [] premises <>
              TeXRaw " What is the conclusion?"
          , back = conclusion }
      (Just _, Nothing) → Just (Left (PremisesWithoutConclusion tex))
      (Nothing, Just _) → Just (Left (ConclusionWithoutPremises tex))
      -- See if the environment seems to contain misformed premises/conclusions
      _ →
        if hasPremises tex ∨ hasConclusion tex
        then Just
               (Left
                 (UserError ("Malformed premises/conclusion in " ++ render title)))
        -- It never had premises or conclusions after all.
        else Nothing

premises ∷ LaTeX → ([Error], [Notecard])
premises = partitionEithers ∘ catMaybes ∘ map premisesAndConclusion ∘ tlc
  where tlc tex = lookForEnvStar "theorem" tex ++
                  lookForEnvStar "lemma" tex ++
                  lookForEnvStar "corrolary" tex
