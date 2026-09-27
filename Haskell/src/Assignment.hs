-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Assignment
  ( bnfParser
  , generateHaskellCode
  , validate
  , ADT(..)
  , getTime
  , saveGenerated
  ) where

import Grammarhask   (bnfParser)
import Genratehask   (generateHaskellCode, validate)  
import Typeshask     (ADT(..))
import Savehask      (getTime, saveGenerated)
