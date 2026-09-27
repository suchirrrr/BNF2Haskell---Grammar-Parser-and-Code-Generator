-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
module Grammarhask (
    bnfParser,
    parse,
    pExtensive,
    pTerminal,
    pTok,
    pGroup,
    pParticle,
    pNonTerm,
    pOptionSet,
    pLeftSide,
    pRuleCore,
    pRuleLine,
    ruleLines
) where

import Control.Applicative (Alternative((<|>)), many, some, empty)
import Control.Monad       (void)
import Instances           (Parser(..))
import Parser
  ( is, isNot, string
  , inlineSpaces, inlineSpace
  , lower, digit
  , eof, upper
  )
import Typeshask


--------------------------------------------------------------------------------
-- Helper for bnfParser
--------------------------------------------------------------------------------

-- Tries to parse p, returning Just result on success,
-- or Nothing if p fails.
-- >>> parse (maybeParser (is 'x')) "x"
-- Result (Just 'x')
maybeParser :: Parser a -> Parser (Maybe a)
maybeParser p = (Just <$> p) <|> pure Nothing

-- Parse a single newline.
-- We use this to make sure rules are written one per line.
--
-- >>> parse eol "\n"
-- Result '\n'
eol :: Parser Char
eol = is '\n'

-- Lets us ignore empty lines between rules.
--
-- >>> parse blankLine "   \n"
-- Result ()
blankLine :: Parser ()
blankLine = void (inlineSpaces *> eol)

-- Parse one-or-more things separated by commas.
-- Spaces around commas are allowed.
--
-- >>> parse (commaSep1 (is 'a')) "a, a, a"
-- Result ['a','a','a']
commaSep1 :: Parser a -> Parser [a]
commaSep1 p =
  (:) <$> p <*> many (inlineSpaces *> is ',' *> inlineSpaces *> p)

-- Check that a list has no repeats.
-- Used to reject <pair(a,a)> because parameters can't repeat 
-- again.
-- >>> unique "abc"
-- True
-- >>> unique "aab"
-- False
unique :: Eq a => [a] -> Bool
unique []     = True
unique (x:xs) = x `notElem` xs && unique xs


--------------------------------------------------------------------------------
-- String literals are handled here
--------------------------------------------------------------------------------


-- handles escape codes inside quoted strings:
--   \"   => actual double quote
--   \\   => backslash
--   \n   => newline char
--   \t   => tab char
escapeCheck :: Parser Char
escapeCheck =
      (is 'n'  *> pure '\n')
  <|> (is 't'  *> pure '\t')
  <|> (is '"'  *> pure '\"')
  <|> (is '\\' *> pure '\\')

-- one character inside a string literal.
-- it's either an escape sequence like \n,
-- or just a normal non-quote character.
charEscape :: Parser Char
charEscape =
      (is '\\' *> escapeCheck)
  <|> (isNot '"')

-- pTerminal:
-- parses a quoted string like "if" or "say\t\"hi\"".
--
-- >>> parse pTerminal "\"hello\""
-- Result (PTerminal "hello")
--
-- >>> parse pTerminal "\"a\\n b\""
-- Result (PTerminal "a\n b")
pTerminal :: Parser Particle
pTerminal =
  PTerminal <$> (is '"' *> many charEscape <* is '"')


--------------------------------------------------------------------------------------------
-- Built-in macros like [int], [alpha] and new features to handle [float], [bool], [ident]
--------------------------------------------------------------------------------------------

-- pExtensive:
-- parses stuff like [int], [alpha], [newline], [float], [bool], [ident]
--
-- >>> parse pExtensive "[int]"
-- Result (PExtensive TInt)
pExtensive :: Parser Particle
pExtensive = do
  _ <- is '['
  ext <-
        (string "int"     *> pure TInt)
    <|> (string "alpha"   *> pure TAlpha)
    <|> (string "newline" *> pure TNewline)
    <|> (string "float"   *> pure TFloat)   -- Part F
    <|> (string "bool"    *> pure TBool)    -- Part F
    <|> (string "ident"   *> pure TIdent)   -- Part F
  _ <- is ']'
  pure (PExtensive ext)


--------------------------------------------------------------------------------
-- This block handles names, parameters and nonterminals
--------------------------------------------------------------------------------

-- parse a rule / nonterminal name.
--
-- >>> parse pName "my_rule1"
-- Result "my_rule1"
pName :: Parser String
pName =
  (:) <$> lower <*> many (lower <|> upper <|> digit <|> is '_')

-- parse a parameter reference like [a] on the RHS,
--
-- >>> parse pParam "[a]"
-- Result (PParam 'a')
pParam :: Parser Particle
pParam =
  PParam <$> (is '[' *> lower <* is ']')


-- parse a nonterminal like <expr>, <term>, <pair(a,b)>
--
-- it can also carry parameters in brackets "(a,b)".
--
-- >>> parse pNonTerm "<term>"
-- Result (PNonTerm "term" Nothing)
--
-- >>> parse pNonTerm "<pair(a,b)>"
-- Result (PNonTerm "pair" (Just [PParam 'a', PParam 'b']))
pNonTerm :: Parser Particle
pNonTerm = do
  _    <- is '<'
  name <- pName
  args <- maybeParser $ do
    _  <- is '('
    xs <- commaSep1 pParticle
    _  <- is ')'
    pure xs
  _ <- is '>'
  pure (PNonTerm name args)


---------------------------------------------------------------------------
-- This block handles tok and (...) groups and implemented upgrades to so 
-- it can wrap grouped alternatives
---------------------------------------------------------------------------

-- parses things like tok "yes", tok [int], tok (<expr> | "x")
-- we allow tok to wrap grouped alternatives like ("yes" | "no").
--
-- >>> parse pTok "tok \"word\""
-- Result (PTok (PTerminal "word"))
--
-- >>> parse pTok "tok [int]"
-- Result (PTok (PExtensive TInt))
pTok :: Parser Particle
pTok = do
  _ <- string "tok"
  _ <- some inlineSpace
  PTok <$> baseParticle

-- parses a parenthesised group of alternatives like:
--   ("yes" | "no")
--   (<digit> <digit> | [ident])
--- >>> parse pGroup "(\"yes\" | \"no\")"
-- Result (PGroup (OptionSet [ParticleGroup [PTerminal "yes"],Particle
pGroup :: Parser Particle
pGroup = do
  _     <- is '('
  _     <- inlineSpaces
  first <- pParticleGroup
  rest  <- many (inlineSpaces *> is '|' <* inlineSpaces *> pParticleGroup)
  _     <- inlineSpaces
  _     <- is ')'
  pure (PGroup (OptionSet (first : rest)))


--------------------------------------------------------------------------------
-- Handles Particles and groups
--------------------------------------------------------------------------------

-- The building blocks that can appear in a production,
-- we also have grouped alternatives via pGroup.
--
-- includes:
--   - "literal"
--   - [int], [float], [ident], etc
--   - <ruleName(...)>
--   - [a]   (param ref)
--   - (...) (grouped alternatives)  
baseParticle :: Parser Particle
baseParticle =
      pTerminal
  <|> pExtensive
  <|> pNonTerm
  <|> pParam
  <|> pGroup

-- One unit given in the RHS of a rule.
-- It can be: tok ... , a baseParticle
-- and we also allow * + ? after it:
--   X*  zero or more
--   X+  one or more
--   X?  optional
--
-- >>> parse pParticle "\"word\"*"
-- Result (PStar (PTerminal "word"))
--
-- >>> parse pParticle "<digit>?"
-- Result (POpt (PNonTerm "digit" Nothing))
pParticle :: Parser Particle
pParticle = do
  core <- pTok <|> baseParticle
  m    <- maybeParser (inlineSpaces *> (is '*' <|> is '+' <|> is '?'))
  pure $ case m of
    Just '*' -> PStar core
    Just '+' -> PPlus core
    Just '?' -> POpt  core
    _        -> core

-- A sequence of particles that can appear next to each other.
-- e.g. `<expr> "+" <term>`
-- becomes ParticleGroup [ <expr> , "+", <term> ].
--
-- we ALSO allow an empty group, which becomes ParticleGroup [].
--
-- >>> parse pParticleGroup "<expr> '+' <term>"
-- Result (ParticleGroup [PNonTerm "expr" Nothing, PTerminal "+", PNonTerm "term" Nothing])
--
-- >>> parse pParticleGroup ""
-- Result (ParticleGroup [])
pParticleGroup :: Parser ParticleGroup
pParticleGroup = do
  _ <- inlineSpaces
  firstMaybe <- maybeParser pParticle
  rest <- case firstMaybe of
            Nothing      -> pure []
            Just firstPt -> (firstPt :) <$> many (some inlineSpace *> pParticle)
  _ <- inlineSpaces
  pure (ParticleGroup rest)

-- Handles `a b c | d e | f`.
-- So a rule body is a list of ParticleGroup branches split by '|'.
--
-- >>> parse pOptionSet "<term> | <term> '+' <expr>"
-- Result (OptionSet [...])
pOptionSet :: Parser OptionSet
pOptionSet = do
    first <- pParticleGroup
    rest  <- many (inlineSpaces *> is '|' <* inlineSpaces *> pParticleGroup)
    pure (OptionSet (first : rest))


--------------------------------------------------------------------------------
-- Handles checking for the parameters used.
--------------------------------------------------------------------------------

-- collectParams:
-- walk a Particle and grab all parameter references like [a], [b], etc.
collectParams :: Particle -> [Char]
collectParams (PParam c)            = [c]
collectParams (PTok p)              = collectParams p
collectParams (PStar p)             = collectParams p
collectParams (PPlus p)             = collectParams p
collectParams (POpt  p)             = collectParams p
collectParams (PNonTerm _ (Just a)) = concatMap collectParams a
collectParams (PGroup (OptionSet gs)) =
  concatMap collectGroupParams gs
collectParams _                     = []

-- This uses the same idea as collectParms, 
-- but for a whole ParticleGroup.
collectGroupParams :: ParticleGroup -> [Char]
collectGroupParams (ParticleGroup ps) =
  concatMap collectParams ps


-- Given the parameter names declared on the left-hand side,
-- make sure we don't use anything else on the right-hand side.
-- if we do, the parser fails here.
--
-- Example:
--   <pair(a,b)> ::= [a] "," [b]    -- OK
--   <bad(a)>    ::= [b]            -- reject, 'b' undeclared
checkParams :: [Char] -> OptionSet -> Parser ()
checkParams allowed (OptionSet gs) =
  let used = concatMap collectGroupParams gs
  in if all (`elem` allowed) used
        then pure ()
        else empty


--------------------------------------------------------------------------------
-- Rules and full grammar parsing
--------------------------------------------------------------------------------

-- parse the left side of a rule:
--   <name>
--   <name(a,b,c)>
--
-- returns (ruleName, paramsDeclared).
--
-- we also make sure param names are unique.
--
-- >>> parse pLeftSide "<term>"
-- Result ("term", [])
--
-- >>> parse pLeftSide "<pair(a,b)>"
-- Result ("pair", ['a','b'])
pLeftSide :: Parser (String, [Char])
pLeftSide = do
  _      <- inlineSpaces
  _      <- is '<'
  name   <- pName
  params <- maybeParser (is '(' *> commaSep1 lower <* is ')')
  _      <- is '>'
  let ps = maybe [] id params
  if unique ps
     then pure (name, ps)
     else empty

-- parse a single grammar rule body:
--
--   <expr> ::= <term> | <term> "+" <expr>
--
-- also runs checkParams to reject bad parameter usage.
pRuleCore :: Parser Rule
pRuleCore = do
    (name, params) <- pLeftSide
    _              <- inlineSpaces *> string "::=" <* inlineSpaces
    body           <- pOptionSet
    _              <- inlineSpaces
    _              <- checkParams params body
    pure Rule
      { ruleName   = name
      , ruleParams = params
      , ruleBody   = body
      }

-- pRuleLine:
-- parse one full line of a rule.
-- we optionally consume the newline at the end.
-- (but we don't force EOF here)
pRuleLine :: Parser Rule
pRuleLine = do
  r <- pRuleCore
  _ <- inlineSpaces
  _ <- maybeParser (void eol) 
  pure r

-- Parses multiple rules in a row,
-- skips blank lines between them.
ruleLines :: Parser [Rule]
ruleLines = do
  r1 <- pRuleLine
  rs <- many (many blankLine *> pRuleLine)
  pure (r1 : rs)

-- Parses the entire grammar file/string:
--   - skip leading blank lines
--   - parse all rules
--   - make sure we reached EOF
pGrammar :: Parser ADT
pGrammar = do
  _     <- many blankLine
  rules <- ruleLines
  _     <- eof
  pure (ADT (Grammar rules))

-- bnfParser:
-- Used to parse a full BNF grammar into an ADT.
bnfParser :: Parser ADT
bnfParser = pGrammar
