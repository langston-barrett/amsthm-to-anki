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

-- Prelude
import ClassyPrelude
import Prelude.Unicode

-- Imported modules
-- import Data.Data (Data)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.LaTeX (render)
import Text.LaTeX.Base.Syntax (LaTeX(..))

-- | The different priority levels for logs
data LogLevel where
  Debug ∷ LogLevel
  Info  ∷ LogLevel
  Warn  ∷ LogLevel
  Error ∷ LogLevel
  deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Logs that will eventually be fed to stdout
data Log where
  Log ∷ LogLevel → Text → Log
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Turn a Log into a Text that can be shown to the user.
showLog ∷ Log → Text
showLog (Log lvl txt) = showLogLevel lvl ++ txt
  where showLogLevel Debug = "[DEBUG] "
        showLogLevel Info  = "[INFO] "
        showLogLevel Warn  = "[WARN] "
        showLogLevel Error = "[ERROR] "

-- | All of the various errors we could encounter in this program.
data Error where
  ErrorStr ∷ Text → Error       -- ^ Generic, should be used rarely
  UserError                 ∷ Text → Error  -- ^ Error in user's LaTeX
  PremisesWithoutConclusion ∷ LaTeX → Error -- ^ What it sounds like
  ConclusionWithoutPremises ∷ LaTeX → Error -- ^ What it sounds like
  EquationNoArguments       ∷ Error         -- ^ What it sounds like
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Turn an Error into a Text that can be shown to the user.
showError ∷ Error → Text
showError e =
  case e of
    ErrorStr s → "Generic error: " ++ s
    UserError u → "Error in the source LaTeX: " ++ pack (show u)
    PremisesWithoutConclusion tex →
      "Encountered premises without conclusion: " ++
      (render tex) ++ "With structure:\n" ++ (pack ∘ show $ tex)
    ConclusionWithoutPremises tex →
      "Encountered conclusion without premises: " ++
      (render tex) ++ "With structure:\n" ++ (pack ∘ show $ tex)
    EquationNoArguments → "\\equation command recieved no arguments"

-- Valid Anki notecard. For simplicity, we only consider the case with 2
-- "faces".
data Notecard where
  Notecard ∷ { front ∷ LaTeX , back ∷ LaTeX } → Notecard
  deriving (Data, Eq, Generic, Show, Typeable)

