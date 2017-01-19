-- |
-- Module      :  Extract.Equations
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Extract equations, create appropriate notecards as described in the
-- README.

module Extract.Equations where

-- Prelude
import ClassyPrelude hiding ((<>))
import Prelude.Unicode

-- Imported modules
import Text.LaTeX
import Text.LaTeX.Base.Syntax

-- My modules
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

-- | Extracts the name, setup, and the inside of the equation from an eqenv
-- environments.
--
-- | Takes `makeDefinitionNote`, `makeTheoremNote`, or `makeLemmaNote` as a first
-- argument.
splitEqEnv ∷ ([TeXArg], LaTeX) → Either Error (LaTeX, LaTeX, LaTeX)
splitEqEnv (args, content) =
  let name =
      case args of
        [OptArg name] → name
        _ → TeXEmpty
    isEquation tex =
      case tex of
        TeXEnv "equation" _ _ → True
        TeXEnv "equation*" _ _ → True
        _ → False
    -- The setup is everything _but_ the equation
    setup = texmap isEquation (\_ → TeXEmpty) content
    
  in
  

-- | Create a valid Anki notecard from the arguments to an \\equation, after
-- they've been checked for validity.
makeEquationNote ∷ (LaTeX, LaTeX, Text) → Notecard
makeEquationNote (name, setup, equality) =
  let firstHalf = takeWhile (/= '=') equality -- everything before "="
  in Notecard { front = setup <> (TeXEnv "equation*" [] $
                                 if name ≢ TeXEmpty ∧ name ≢ TeXRaw ""
                                 then TeXSeq
                                        (TeXComm "tag" [FixArg name])
                                        (TeXRaw (firstHalf ++ "="))
                                 else TeXRaw (firstHalf ++ "="))
              , back = setup <> TeXEnv "equation*" [] (TeXRaw equality)
              }

equations ∷ LaTeX → ([Error], [Notecard])
equations =
  let eqenvs = lookForEnvStar "eqenv"
  partitionEithers ∘ map (fmap makeEquationNote) ∘
                     map checkEquationArgs ∘ lookForCommand "equation"
