-- |
-- Module      :  Extract
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Ties together the Extract submodules, provides a cohesive API that creates a
-- full LaTeX document.
-- Required for Eff class
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Unsafe #-}

-- "import safe" is enforced.
module Extract where

import ClassyPrelude hiding (lift, (<>))
import Control.Arrow (left)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Writer.Strict
import Text.LaTeX hiding (lift)
import Text.LaTeX.Base.Syntax

import Extract.DefinitionsTheoremsLemmas
       (definitionsTheoremsLemmas)
import Extract.Equations (equations)
import Extract.Notes (notes)
import Extract.Premises (premises)

-- My modules
import Types

-- | Turns a single notecard into a LaTeX block in the obvious way.
notecardToLaTeX :: Notecard -> LaTeX
notecardToLaTeX notecard =
  TeXEnv "note" [] $
  TeXEnv "field" [] (front notecard) <> TeXEnv "field" [] (back notecard)

-- | The high level logic of the program: Extract and transform notecards,
-- return the final document (all with error handling and logging!).
--
-- A note on the type: this function uses the extensible-effects library, and
-- the type specifies that it must have access to a Writer monad.
textToNotecards
  :: (Member (Writer [Log]) r)
  => LaTeX -> Eff r ([Error], LaTeX)
textToNotecards srcTex =
  let results =
        [ definitionsTheoremsLemmas srcTex
        , equations srcTex
        , notes srcTex
        , premises srcTex
        ] :: [([Error], [Notecard])]
  in pure
       ( concat . map fst $ results
       , concat . map notecardToLaTeX . concat . map snd $ results)

-- | Takes a body and creates a simple document
doc :: LaTeX -> LaTeX
doc str =
  documentclass [] article <> title "Generated Anki Notecards" <>
  author "siddharthist" <>
  document (maketitle <> str)
