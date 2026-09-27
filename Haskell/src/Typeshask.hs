-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
module Typeshask
  ( ADT(..), Grammar(..), Rule(..)
  , OptionSet(..), ParticleGroup(..), Particle(..)
  , ExtType(..)
  ) where

-- Root grammar structure
data ADT = ADT Grammar
  deriving (Show)

newtype Grammar = Grammar [Rule]
  deriving (Show)

data Rule = Rule
  { ruleName   :: String
  , ruleParams :: [Char]
  , ruleBody   :: OptionSet
  } deriving (Show)

newtype OptionSet     = OptionSet [ParticleGroup] deriving (Show)
newtype ParticleGroup = ParticleGroup [Particle]  deriving (Show)

-- Grammar elements and built-in types
data Particle
  = PTerminal String
  | PExtensive ExtType            
  | PNonTerm  String (Maybe [Particle]) 
  | PParam    Char                
  | PGroup    OptionSet           
  | PTok      Particle           
  | PStar     Particle            
  | PPlus     Particle            
  | POpt      Particle           
  deriving (Show)

data ExtType
  = TInt
  | TAlpha
  | TNewline
  | TFloat     -- [float]
  | TBool      -- [bool]
  | TIdent     -- [ident]
  deriving (Show)
 