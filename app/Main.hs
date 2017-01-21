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
import Text.LaTeX.Base.Pretty (prettyLaTeX)

-- My modules
import Extract
import Types

data Format where
  LaTeXFormat :: Format
  TextFormat :: Format
    deriving (Data, Eq, Generic, Show, Read, Typeable)

data Options where
        Options ::
          {inputPath :: String, outputPath :: String, outputFormat :: Format,
           pretty :: Bool}
          -> Options
    deriving (Data, Eq, Generic, Show, Read, Typeable)

amsthmToAnki :: Options -> IO ()
amsthmToAnki (Options inputPath outputPath format pretty) = do
  content <- readFile inputPath
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
          -- renderFile (outputPath opts) tex

        -- Write the output file!
        if pretty
        then do
          putStrLn "___ pretty +++"
          writeFile outputPath (prettyLaTeX tex)
        else do
          putStrLn "___ not pretty +++"
          renderFile outputPath tex

    -- TODO: pretty-print?
    Left err -> putStrLn ("[ERROR] " ++ pack (show err))

-- | Define the option parser and pass control along to amsthmToAnki.
main :: IO ()
main =
  amsthmToAnki =<<
  (execParser $
   info
     (helper <*>
      (Options <$> inputPath <*> outputPath <*> outputFormat <*> pretty))
     (fullDesc <>
      progDesc "Translate math notes written with amsthm to Anki notecards." <>
      header "amsthm-to-anki"))
  where
    inputPath :: Parser String
    inputPath = argument str $ metavar "INPUT" <> help "the input .tex file"
    -- TODO use stdout if unspecified
    outputPath :: Parser String
    outputPath =
      argument str $
      metavar "OUTPUT" <> help "the .txt or .tex file to output to"
    -- TODO: make this do something
    outputFormat :: Parser Format
    outputFormat =
      option auto $
      long "format" <> metavar "f" <> value LaTeXFormat <>
      help "Output in native Anki txt or the LaTeX Note Importer format"
    pretty :: Parser Bool
    pretty = switch $ long "pretty" <> help "Pretty-print LaTeX output"
