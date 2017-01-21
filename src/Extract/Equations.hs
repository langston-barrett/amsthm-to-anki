-- |
-- Module      :  Extract.Equations
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Extract equations, create appropriate notecards as described in the
-- README.
-- "forall a."
{-# LANGUAGE ExplicitForAll #-}

module Extract.Equations where

import ClassyPrelude hiding ((<>))
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Data.List.Split (splitOn)

-- My modules
import Extract.Util
import Types

-- | Descend a LaTeX document tree, applying the function at every level down to
-- the leaves, collecting the results. Only applies recursively in the case of
-- TeXEnv, TeXSeq, and TeXBraces, which are the two constructors that contain sub-latex.
descendTeX
  :: forall a.
     (LaTeX -> a) -> LaTeX -> [a]
descendTeX f tex =
  let helper g tex =
        case tex of
          TeXBraces child -> g tex : helper g child
          TeXEnv _ _ child -> g tex : helper g child
          TeXMath _ child -> g tex : helper g child
          TeXSeq child1 child2 -> (g tex : helper g child1) ++ (helper g child2)
          _ -> [g tex]
  in helper f tex

-- | Make a list of all of the levels of the LaTeX tree.
flattenTeX :: LaTeX -> [LaTeX]
flattenTeX = descendTeX id

-- | Extracts the name, setup, and the inside of the equation from an eqenv
-- environment.
splitEqEnv :: ([TeXArg], LaTeX) -> Either Error (Maybe LaTeX, LaTeX, LaTeX)
splitEqEnv (args, content) =
  let isEquation :: LaTeX -> Bool
      isEquation tex =
        case tex of
          TeXEnv "equation" _ _ -> True
          TeXEnv "equation*" _ _ -> True
          _ -> False
  -- First, extract the inside of the equation (because this might fail,
  -- and we'll have to throw a Left
  in case (lookForEnvStar "equation" content) of
       [(_, inside)] ->
         Right
           ( case args of
               [OptArg name] -> Just name
               _ -> Nothing
           -- The setup is everything _but_ the equation
           , texmap isEquation (\_ -> TeXEmpty) content
           , inside)
        -- TODO: better error messages
       [] -> Left (UserError "No equation env in eqenv!")
       _ -> Left (UserError "Too many equation(s) in eqenv!")

-- | Create a valid Anki notecard from the arguments to an \\equation, after
-- they've been checked for validity.
makeEquationNote :: (Maybe LaTeX, LaTeX, LaTeX) -> Either Error Notecard
makeEquationNote (maybeName, setup, equality) =
  case splitOn " = " (unpack . render $ equality) of
    [firstHalf, _] ->
      Right $
      Notecard
      { front =
          setup <>
          (TeXEnv "equation*" [] $
           case maybeName of
             Just name ->
               TeXSeq
                 (TeXComm "tag" [FixArg name])
                 (TeXRaw (pack $ firstHalf ++ " = "))
             Nothing -> TeXRaw (pack $ firstHalf ++ " = "))
      , back = setup <> TeXEnv "equation*" [] equality
      }
    -- TODO: better errors
    x -> Left (CouldntSplitEquality x)

-- TODO: improve naming, etc
equations :: LaTeX -> ([Error], [Notecard])
equations tex =
  let (errs1, results1) =
        partitionEithers . map splitEqEnv . lookForEnvStar "eqenv" $ tex
      (errs2, results2) = partitionEithers . map makeEquationNote $ results1
  in (errs1 ++ errs2, results2)
