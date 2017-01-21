-- |
-- Module      :  Types
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- The types used everywhere in this program. Imports nothing, is imported
-- everywhere.
module Types
  ( Error(..)
  , Log(..)
  , LogLevel(..)
  , Notecard(..)
  , showError
  , showLog
  ) where

import ClassyPrelude
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.LaTeX (render)
import Text.LaTeX.Base.Syntax (LaTeX(..))

-- | The different priority levels for logs
data LogLevel where
        Debug :: LogLevel
        Info :: LogLevel
        Warn :: LogLevel
        Error :: LogLevel
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Logs that will eventually be fed to stdout
data Log where
        Log :: LogLevel -> Text -> Log
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Turn a Log into a Text that can be shown to the user.
showLog :: Log -> Text
showLog (Log lvl txt) = showLogLevel lvl ++ txt
  where
    showLogLevel Debug = "[DEBUG] "
    showLogLevel Info = "[INFO] "
    showLogLevel Warn = "[WARN] "
    showLogLevel Error = "[ERROR] "

-- | All of the various errors we could encounter in this program.
data Error where
        ErrorStr :: Text -> Error
        UserError :: Text -> Error
        PremisesWithoutConclusion :: LaTeX -> Error
        ConclusionWithoutPremises :: LaTeX -> Error
        EquationNoArguments :: Error
        CouldntSplitEquality :: [[Char]] -> Error
    deriving (Data, Eq, Generic, Show, Typeable)

-- | Turn an Error into a Text that can be shown to the user.
showError :: Error -> Text
showError e =
  let
    texToText :: forall a. (Show a) => a -> Text
    texToText = pack . show
  in case e of
       ErrorStr s -> "Generic error: " ++ s
       UserError u -> "Error in the source LaTeX: " ++ u
       PremisesWithoutConclusion tex ->
         "Encountered premises without conclusion: " ++
         (render tex) ++ "With structure:\n" ++ texToText tex
       ConclusionWithoutPremises tex ->
         "Encountered conclusion without premises: " ++
         (render tex) ++ "With structure:\n" ++ texToText tex
       EquationNoArguments -> "\\equation command recieved no arguments"
       CouldntSplitEquality x ->
         "Couldn't split equality into first and second halves. Note " ++
         "that this requires the substring ' = ' to appear between the halves. " ++
         texToText x

-- Valid Anki notecard. For simplicity, we only consider the case with 2
-- "faces".
data Notecard where
        Notecard :: {front :: LaTeX, back :: LaTeX} -> Notecard
    deriving (Data, Eq, Generic, Show, Typeable)
