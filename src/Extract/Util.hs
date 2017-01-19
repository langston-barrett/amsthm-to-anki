-- |
-- Module      :  Extract.Util
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Functions that are useful throughout the Extract submodules.
module Extract.Util where

import ClassyPrelude hiding ((<>))
import Data.Either (partitionEithers)
import Text.LaTeX
import Text.LaTeX.Base.Syntax

-- My modules
import Types

-- | Just like lookForEnv, but (for example), when looking for "equation", it
-- also looks for "equation*"
lookForEnvStar :: String -> LaTeX -> [([TeXArg], LaTeX)]
lookForEnvStar env tex = lookForEnv env tex ++ lookForEnv (env ++ "*") tex
