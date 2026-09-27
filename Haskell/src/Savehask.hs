-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
module Savehask (
    getTime,
    saveGenerated,
    firstRuleName
) where

import Data.Time      (formatTime, defaultTimeLocale, getCurrentTime)
import Typeshask
import Genratehask    (generateHaskellCode)

--------------------------------------------------------------------------------
-- This module handles saving the generated Haskell code to disk.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Choosing the output filename
--------------------------------------------------------------------------------

-- firstRuleName:
-- Look at the first rule in the grammar and use that as a base filename.
-- example if first rule is <expr> we will save "expr.hs".
--
-- If the grammar is empty (no rules), we fall back to "output.hs"
-- so that the generator still works instead of crashing.
--
-- >>> firstRuleName (ADT (Grammar [Rule "term" [] (OptionSet [])]))
-- "term"
--
-- >>> firstRuleName (ADT (Grammar []))
-- "output"
firstRuleName :: ADT -> String
firstRuleName (ADT (Grammar (Rule n _ _ : _))) = n
firstRuleName _                                = "output"


--------------------------------------------------------------------------------
-- The header we stick at the top of the generated file
--------------------------------------------------------------------------------

-- We build the text that goes at the top of the output module.
-- This defines:
--   - a module name: Output
--   - imports for Parser / Instances
--   - helper utilities runParser, trailing, lexeme
genHeader :: String -> String
genHeader timestamp = unlines
  [ "{-"
  , "  Generated at: " ++ timestamp
  , "  NOTE: This file is auto-generated from the grammar."
  , "        The parsers for [float], [ident], etc. are expected"
  , "        to live in your runtime Parser environment."
  , "-}"
  , "module Output where"
  , ""
  , "-- basic parser machinery we rely on"
  , "import Control.Applicative"
  , "    ( many, optional, some"
  , "    , (<|>)"
  , "    , (<$>)"
  , "    , (<*)"
  , "    , (<*>)"
  , "    )"
  , "import Instances (ParseResult (..), Parser (..))"
  , "import Parser"
  , "    ( alpha"
  , "    , charTok"
  , "    , eof"
  , "    , int"
  , "    , is"
  , "    , string"
  , "    , stringTok"
  , "    , tok"
  , "    -- NOTE: we assume float, ident, etc. are also provided"
  , "    )"
  , ""
  , "-- keep Prelude minimal so we don't clash with generated names"
  , "import Prelude"
  , "    ( Char, Int, Double, Bool"
  , "    , Maybe, Show, String"
  , "    , show, (==), reverse"
  , "    , pure, (<$>), (<*>), (>>=)"
  , "    )"
  , ""
  , "-- runParser:"
  , "-- Helper for testing a generated parser at runtime."
  , "-- We run the parser, then 'show' the value if it succeeded,"
  , "-- otherwise return \"Parse Error\" so it's printable."
  , "runParser :: Show a => Parser a -> String -> String"
  , "runParser p s = case parse (p <* trailing <* eof) s of"
  , "    Result _ a -> show a"
  , "    Error _    -> \"Parse Error\""
  , ""
  , "-- trailing:"
  , "-- Skip any extra whitespace/newlines at the end"
  , "-- so the parsers are not super strict about EOF position."
  , "trailing :: Parser ()"
  , "trailing = () <$ many (is ' ' <|> is '\\t' <|> is '\\r' <|> is '\\n')"
  , ""
  , "-- lexeme:"
  , "-- Typical combinator used in parser libs: parse p, then eat trailing."
  , "-- This lets you attach automatic whitespace skipping when needed."
  , "lexeme :: Parser a -> Parser a"
  , "lexeme p = p <* trailing"
  , ""
  ]


--------------------------------------------------------------------------------
-- writing the new file
--------------------------------------------------------------------------------

-- saveGenerated:
-- Actually writes the file.
--
-- Steps followed:
--   1. get timestamp
--   2. pick filename (based on first rule)
--   3. build header text (module Output + helpers)
--   4. append the generated AST code from Genratehask
--   5. write it to disk
--
-- It returns the file path we wrote, and also prints a message.
saveGenerated :: FilePath -> ADT -> IO FilePath
saveGenerated outDir adt = do
  ts <- getTime
  let baseName = firstRuleName adt ++ ".hs"
      outFile  = if null outDir
                   then baseName
                   else outDir ++ "/" ++ baseName
      fileBody = genHeader ts ++ generateHaskellCode adt

  writeFile outFile fileBody
  putStrLn ("Saved generated file: " ++ outFile)
  pure outFile


--------------------------------------------------------------------------------
-- timestamp helper
--------------------------------------------------------------------------------

-- getTime:
-- Returns something like "2025-10-25T22-18-59".
-- purely for logging/debugging in the header.
getTime :: IO String
getTime =
  formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S"
    <$> getCurrentTime
