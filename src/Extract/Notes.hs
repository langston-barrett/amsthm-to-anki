-- |
-- Module      :  Extract.Notes
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Extract ready-to-go notes, and reproduce them in the final document as
-- described in the README. This is essentially an elaborate identity function
-- on blocks of LaTeX source that represent note environments.
module Extract.Notes where

-- Prelude
import ClassyPrelude hiding ((<>))

-- Imported modules
import Text.LaTeX
import Text.LaTeX.Base.Syntax

-- My modules
import Types

-- | Takes the inside of a "note" environment, ensures it has two "fields", and
-- returns a Notecard with the same content.
noteID :: LaTeX -> Either Error Notecard
noteID ntex =
  case (lookForEnv "field" ntex) of
    [([], field1), ([], field2)] ->
      Right $ Notecard {front = field1, back = field2}
    [(_, _), (_, _)] ->
      Left $ UserError "Malformed note environment: fields don't take args"
    [] -> Left $ UserError "Malformed note environment: no fields found"
    _ -> Left $ UserError "Malformed note environment"

notes :: LaTeX -> ([Error], [Notecard])
notes = partitionEithers . map (noteID . snd) . lookForEnv "note"
