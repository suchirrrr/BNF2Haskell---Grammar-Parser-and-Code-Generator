-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
module Genratehask (
    generateHaskellCode,
    validate,
    genType,
    combineAlternatives,
    makeParser
) where

import Data.List    (groupBy, sortBy)
import Data.Char    (toUpper)
import Instances    (Parser(..))
import Typeshask
import Validatehask (validate, cleanADT)

--------------------------------------------------------------------------------
-- This is the main code generator:
-- Takes the given parsed grammar (ADT) and turn it into real Haskell code,
-- merges duplicate rules (<expr> defined multiple times etc.). For each rule we
-- generate its data / newtype. For each rule we generate its parser function

--------------------------------------------------------------------------------

-- Given the ADT for the whole grammar,
-- return a String which is basically the full Haskell module body.
generateHaskellCode :: ADT -> String
generateHaskellCode adt =
  let (ADT (Grammar rules0)) = adt
      mergedRules            = combineAlternatives rules0
      bodyLines =
           concatMap genType    mergedRules
        ++ concatMap makeParser mergedRules
        ++ [""]
  in unlines bodyLines



--------------------------------------------------------------------------------
-- Merging rules with the same name
--------------------------------------------------------------------------------

-- If we write:
--   <expr> ::= <term>
--   <expr> ::= <term> "+" <expr>
-- we don't want two different Haskell functions called "expr".
-- so we combine them into one Rule with both branches.
combineAlternatives :: [Rule] -> [Rule]
combineAlternatives rs =
  map groupMerge grouped
  where
    grouped = groupBy sameRule $ sortBy cmpRule rs

    sameRule r1 r2 =
      ruleName r1   == ruleName r2 &&
      ruleParams r1 == ruleParams r2

    cmpRule r1 r2 =
      compare (ruleName r1, ruleParams r1)
              (ruleName r2, ruleParams r2)

    groupMerge []     = error "groupMerge: no rules in group (should not happen)"
    groupMerge (r:xs) =
      Rule (ruleName r) (ruleParams r)
        (OptionSet (concatMap ruleBodyOptions (r:xs)))

    ruleBodyOptions rule =
      case ruleBody rule of
        OptionSet ps -> ps


--------------------------------------------------------------------------------
-- basic name helpers
--------------------------------------------------------------------------------

-- Capitalise for type/constructor names.
-- "expr" -> "Expr"
cap :: String -> String
cap ""     = ""
cap (x:xs) = toUpper x : xs

-- turn "ab" -> ["a","b"]
-- we'll use those as type variables, e.g. Parser A -> Parser B -> ...
typeupper :: [Char] -> [String]
typeupper = map pure

-- We build the signature of the parser function.
--
-- without params:
--   expr :: Parser Expr
--
-- with params "ab":
--   pair :: Parser a -> Parser b -> Parser (Pair a b)
proptype :: String -> [Char] -> String
proptype name ps =
  case typeupper ps of
    []  ->
      name ++ " :: Parser " ++ cap name
    tvs ->
      name ++ " :: "
      ++ concatMap (\tv -> "Parser " ++ tv ++ " -> ") tvs
      ++ "Parser (" ++ unwords (cap name : tvs) ++ ")"


--------------------------------------------------------------------------------
-- literal escaping for codegen (extra feature)
--------------------------------------------------------------------------------

-- escape one char when turning a string literal into Haskell code.
escCharOut :: Char -> String
escCharOut '"'  = "\\\""
escCharOut '\\' = "\\\\"
escCharOut '\n' = "\\n"
escCharOut '\t' = "\\t"
escCharOut c    = [c]

-- Takes a raw string 
-- and returns a quoted/escaped Haskell string literal.
-- Example:  hello\n -> "hello\n"
emitLiteral :: String -> String
emitLiteral s =
  "\"" ++ concatMap escCharOut s ++ "\""


--------------------------------------------------------------------------------
-- Figuring out Haskell types for particles
--------------------------------------------------------------------------------

-- Helper to check types  .
whatType :: ExtType -> String
whatType TInt     = "Int"
whatType TAlpha   = "String"
whatType TNewline = "Char"
whatType TFloat   = "Double"  --new features
whatType TBool    = "Bool"  
whatType TIdent   = "String" 

-- Helper for grouped (...) types.
firstAltType :: [ParticleGroup] -> String
firstAltType [] = "()"
firstAltType (ParticleGroup ps : _) =
  case ps of
    []    -> "()"
    (x:_) -> particleType x

-- Helper for q* and q+
listType :: Particle -> String
listType q = "[" ++ particleType q ++ "]"

-- Guess the result type of a particle.
-- (We need this when generating data/newtype field types.)
--
-- terminals -> String
-- PExtensive -> Int / Double / etc (including our Part F stuff)
-- groups (...) -> take first branch and infer from that
-- param [a] -> "a"
-- <rule> -> RuleName, plus its type args
-- X* / X+ -> [TypeOfX]
-- X? -> Maybe TypeOfX
particleType :: Particle -> String
particleType p =
  case p of
    PTerminal _  -> "String"
    PExtensive e -> whatType e
    PParam c     -> [c]
    PGroup (OptionSet alts) -> firstAltType alts
    PTok q -> particleType q
    PStar q -> listType q
    PPlus q -> listType q
    POpt q -> "Maybe " ++ particleType q
    PNonTerm n mp ->
      case mp of
        Nothing ->
          cap n
        Just ps ->
          unwords (cap n : map particleType ps)


--------------------------------------------------------------------------------
-- Building parser expressions for particles 
--------------------------------------------------------------------------------

-- For things like [int], [float], [bool], [ident]
--  * TFloat uses "float"
--  * TBool parses "true" or "false"
--  * TIdent uses "ident"
parserForExt :: ExtType -> String
parserForExt ext =
  case ext of
    TInt     -> "int"
    TAlpha   -> "some alpha"
    TNewline -> "is '\\n'"
    TFloat   -> "float" 
    TBool    -> "(string \"true\" <|> string \"false\")"
    TIdent   -> "ident"  

-- Build a parser for multiple particles in a single alternative group.
-- Example:
--   [p1,p2] becomes (\a b -> (a,b)) <$> p1 <*> p2
--
tupleChain :: [Particle] -> String
tupleChain xs =
  case xs of
    [a,b] ->
      "(\\a b -> (a,b)) <$> "
      ++ particleExpr a
      ++ " <*> "
      ++ particleExpr b
    (a:b:c:rest) ->
      "(\\a b c -> ((a,b),c)) <$> "
      ++ particleExpr a
      ++ " <*> "
      ++ particleExpr b
      ++ " <*> "
      ++ particleExpr c
      ++ concatMap (\q -> " <*> " ++ particleExpr q) rest
    _ ->
      "pure ()" 

-- Turn ONE alternative (ParticleGroup [...]) into code.
-- cases:
--   []           -> pure ()
--   [single]     -> particleExpr single
--   [a,b,c,...]  -> tupleChain ...
groupAltExpr :: ParticleGroup -> String
groupAltExpr (ParticleGroup ps) =
  case ps of
    []     -> "pure ()"
    [one]  -> particleExpr one
    manyPs -> "( " ++ tupleChain manyPs ++ " )"

-- Turn ("yes" | "no" | <something>) into one parser expression string.
-- We produce something like:
--   ( <alt1> <|> <alt2> <|> <alt3> )
groupExpr :: [ParticleGroup] -> String
groupExpr alts =
  case alts of
    []     -> "(pure ())"
    (g:gs) ->
      "("
      ++ groupAltExpr g
      ++ concatMap (\other -> " <|> " ++ groupAltExpr other) gs
      ++ ")"

-- if it's tok "foo" we want (stringTok "foo")
-- otherwise (tok <innerParser>)
tokExpr :: Particle -> String
tokExpr inner =
  case inner of
    PTerminal s ->
      "(stringTok " ++ emitLiteral s ++ ")"
    other ->
      "(tok " ++ particleExpr other ++ ")"

-- The actual "emit parser code" function.
-- This is how the generated module will parse values.
particleExpr :: Particle -> String
particleExpr p =
  case p of
    PTerminal s ->
      "(string " ++ emitLiteral s ++ ")"
    PExtensive ext ->
      parserForExt ext
    PParam c ->
      [c]
    PGroup (OptionSet alts) ->
      groupExpr alts
    PTok inner ->
      tokExpr inner
    PStar q ->
      "(many "     ++ particleExpr q ++ ")"
    PPlus q ->
      "(some "     ++ particleExpr q ++ ")"
    POpt q ->
      "(optional " ++ particleExpr q ++ ")"
    PNonTerm n Nothing ->
      n
    PNonTerm n (Just ps) ->
      "(" ++ unwords (n : map particleExpr ps) ++ ")"


--------------------------------------------------------------------------------
-- data/newtype generation
--------------------------------------------------------------------------------

-- Each rule gets either:
--   newtype Foo = Foo <someth> or
--   data Foo = Foo1 ... | Foo2 ...  deriving Show
--
-- rule with exactly 1 alternative AND exactly 1 particle:
--   newtype
--
-- otherwise:
--   data ... with numbered constructors Foo1, Foo2, ...
genType :: Rule -> [String]
genType (Rule name params (OptionSet opts)) =
  case opts of
    [ParticleGroup [p]] -> makeNewtype name params p
    _                   -> makeData    name params opts


-- newtype Foo a b = Foo (TypeHere)
--     deriving Show
makeNewtype :: String -> [Char] -> Particle -> [String]
makeNewtype name params p =
  [ "newtype " ++ cap name ++ tyVarsDecl ++ " = " ++ cap name ++ " " ++ rhsFinal
  , "    deriving Show"
  , ""
  ]
  where
    tyVars     = typeupper params
    tyVarsDecl = case tyVars of
                   []  -> ""
                   tvs -> ' ' : unwords tvs

    rhsType    = particleType p
    rhsFinal
      | any (== ' ') rhsType = "(" ++ rhsType ++ ")"
      | otherwise            = rhsType


-- build constructor "Foo2 a b c"
dataBodyCtor :: String -> Int -> ParticleGroup -> String
dataBodyCtor name idx (ParticleGroup ps) =
  cap name ++ show idx ++ ctorFields ps
  where
    ctorFields [] = ""
    ctorFields xs =
      " " ++ unwords (map wrapIfSpaced (map particleType xs))

    wrapIfSpaced t
      | ' ' `elem` t = "(" ++ t ++ ")"
      | otherwise    = t

-- makeData:
-- data Foo a b = Foo1 Int
--              | Foo2 Int String
--     deriving Show
makeData :: String -> [Char] -> [ParticleGroup] -> [String]
makeData _    _      []      = []
makeData name params (g1:gs) =
  [ "data " ++ headDecl ++ " = " ++ dataBodyCtor name 1 g1 ]
  ++ [ indentPad ++ "| " ++ dataBodyCtor name i g
     | (i,g) <- zip [2 :: Int ..] gs
     ]
  ++ [ "    deriving Show"
     , ""
     ]
  where
    tvars =
      case typeupper params of
        []  -> ""
        tvs -> ' ' : unwords tvs

    headDecl = cap name ++ tvars

    indentPad =
      replicate (length ("data " ++ headDecl ++ " ")) ' '


--------------------------------------------------------------------------------
-- parser function generation
--------------------------------------------------------------------------------

-- Build something like:
--   Foo2 <$> p1 <*> p2 <*> p3
-- or, if no fields:
--   pure (Foo2)
seqApply :: String -> ParticleGroup -> String
seqApply ctor (ParticleGroup ps) =
  case map particleExpr ps of
    []     -> "pure (" ++ ctor ++ ")"
    (p:qs) -> ctor ++ " <$> " ++ p
                  ++ concatMap (\q -> " <*> " ++ q) qs

-- figure out how much space to indent `<|>` branches
-- under the first line.
spacePad :: String -> [String] -> String
spacePad fname args =
  replicate (length (unwords (fname : args)) + 1) ' '

-- Header of the function:
-- e.g. "expr :: Parser Expr"
fnHeader :: String -> [Char] -> String
fnHeader nm ps =
  proptype nm ps

-- left side of the function def:
--   expr
--   pair a b
fnLhs :: String -> [Char] -> String
fnLhs nm ps =
  unwords (nm : map pure ps)

-- For rules with only 1 alternative.
-- If that alt only has 1 particle, we do:
--   Expr <$> <parser>
-- else we do:
--   Expr1 <$> ...
bodySingle :: String -> ParticleGroup -> String
bodySingle name (ParticleGroup [p]) =
  cap name ++ " <$> " ++ particleExpr p
bodySingle name grp =
  seqApply (cap name ++ "1") grp

-- For the first alternative in a multi-alt rule.
-- We always call constructor 1 here.
firstAltBody :: String -> ParticleGroup -> String
firstAltBody name grp =
  seqApply (cap name ++ "1") grp

-- restAltBodies:
-- For the remaining alternatives in a multi-alt rule.
-- We indent each with `<|>`, and use constructor 2, 3, ...
restAltBodies :: String -> String -> [ParticleGroup] -> [String]
restAltBodies name pad rest =
  [ pad ++ "<|> " ++ seqApply (cap name ++ show i) g
  | (i,g) <- zip [2 :: Int ..] rest
  ]

-- buildSingleParser:
-- Emits parser code for a rule that has exactly ONE alternative.
--
-- expr :: Parser Expr
-- expr = Expr <$> term
buildSingleParser :: String -> [Char] -> ParticleGroup -> [String]
buildSingleParser name params grp =
  [ fnHeader name params
  , fnLhs name params ++ " = " ++ bodySingle name grp
  , ""
  ]

-- buildMultiParser:
-- Emits parser code for a rule that has MULTIPLE alternatives:
--
-- expr :: Parser Expr
-- expr = Expr1 <$> term
--       <|> Expr2 <$> term <*> (string "+") <*> expr
buildMultiParser :: String -> [Char] -> ParticleGroup -> [ParticleGroup] -> [String]
buildMultiParser name params first rest =
  let pad = spacePad name (map pure params)
  in  [ fnHeader name params
      , fnLhs name params ++ " = " ++ firstAltBody name first
      ]
      ++ restAltBodies name pad rest
      ++ [""]

-- makeParser:
-- top-level dispatcher:
-- if no options -> nothing
-- if one option -> buildSingleParser
-- if many       -> buildMultiParser
makeParser :: Rule -> [String]
makeParser (Rule name params (OptionSet opts)) =
  case opts of
    []      -> []
    [g]     -> buildSingleParser name params g
    g1:rest -> buildMultiParser  name params g1 rest
