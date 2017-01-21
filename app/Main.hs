-- |
-- Module      :  Main
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Just a basic lib for Anki translation.

-- "import safe" is enforced.
{-# LANGUAGE Unsafe #-} -- "import safe" is enforced
module Main where

import ClassyPrelude hiding ((<>), lift)
import safe Control.Eff
import safe Control.Eff.Lift
import safe Control.Eff.Writer.Strict
import safe Data.Data (Data)
import safe Data.OpenUnion (SetMember)
import safe Data.Typeable (Typeable)
import safe Options.Applicative
import Text.LaTeX (renderFile)
import Text.LaTeX.Base.Parser (parseLaTeX)

-- My modules
import Extract
import Types

description :: String
description = "Translate my math notes into flashcards"

data Options where
        Options :: {inputPath :: String, outputPath :: String} -> Options
    deriving (Data, Eq, Generic, Show, Typeable)

optParser :: Parser Options
optParser =
  Options <$> argument str (metavar "INPUT") <*> argument str (metavar "OUTPUT")

main :: IO ()
main = do
  opts <-
    execParser $
    info
      (helper <*> optParser)
      (fullDesc <> progDesc description <> header "amsthm-to-anki")
  content <- readFile (inputPath opts)
  case (parseLaTeX content) of
    Right srcTeX ->
      let (logs :: [Log], (errs :: [Error], tex)) =
            run (runMonoidWriter (textToNotecards srcTeX))
      in do
        -- Show debug log
        when (logs /= []) $ do
          putStrLn ("~~~ DEBUG/INFO LOG ~~~" :: Text)
          mapM_ putStrLn (map showLog logs)

        -- Show errors
        when (errs /= []) $ do
          hPutStrLn stderr ("~~~ ERRORS ~~~" :: Text)
          mapM_ (hPutStrLn stderr) (map showError errs)
          renderFile (outputPath opts) tex
    -- TODO: pretty-print?
    Left err -> putStrLn ("[ERROR] " ++ pack (show err))
