-- |
-- Module      :  Extract
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Ties together the Extract submodules, provides a cohesive API that creates a
-- full LaTeX document.

module Extract where
-- Prelude
import ClassyPrelude hiding ((<>), lift)
import Prelude.Unicode

-- Imported modules
import Control.Arrow (left)
import Text.LaTeX hiding (lift)
import Text.LaTeX.Base.Syntax
import safe Control.Eff
import safe Control.Eff.Lift
import safe Control.Eff.Writer.Strict

-- My modules
import Types
import Extract.DefinitionsTheoremsLemmas (definitionsTheoremsLemmas)
import Extract.Equations (equations)
import Extract.Notes (notes)
import Extract.Premises (premises)

-- | Turns a single notecard into a LaTeX block in the obvious way.
notecardToLaTeX ∷ Notecard → LaTeX
notecardToLaTeX notecard =
  TeXEnv "note" [] $
    TeXEnv "field" [] (front notecard) <>
    TeXEnv "field" [] (back notecard)

-- | The high level logic of the program: Extract and transform notecards,
-- return the final document (all with error handling and logging!).
--
-- A note on the type: this function uses the extensible-effects library, and
-- the type specifies that it must have access to a Writer monad.
textToNotecards ∷ (Member (Writer [Log]) r) ⇒ LaTeX → Eff r ([Error], LaTeX)
textToNotecards srcTex =
  let results = [ definitionsTheoremsLemmas srcTex
                , equations srcTex
                , notes srcTex
                , premises srcTex
                ] ∷ [([Error], [Notecard])]
  in pure (concat ∘ map fst $ results,
           concat ∘ map notecardToLaTeX ∘ concat ∘ map snd $ results)

-- | Takes a body and creates a simple document
doc :: LaTeX → LaTeX
doc str =
  documentclass [] article
  <> title "Generated Anki Notecards"
  <> author "siddharthist"
  <> document (maketitle <> str)
