-- AI Usage Declaration:
-- Selected sections of this file (such as parser validation and grammar extension ideas) were created or adapted using Perplexity AI (October 2025).
-- All AI-generated suggestions have been modified, implemented, and tested by the author.
module Validatehask
  ( validate
  , cleanADT
  ) where

import Typeshask

--------------------------------------------------------------------------------
-- this module is for validating the grammar ADT
-- and producing human-readable warnings. 
--------------------------------------------------------------------------------


-- validate:
-- given an ADT, produce a list of warning messages about problems found.
-- order of messages:
--   left recursion first (dangerous loops)
--   undefined nonterminals
--   duplicate rule names
--
-- Example shape:
--   ["Left recursion in: expr"
--   ,"Undefined nonterminal: term"
--   ,"Duplicate rule: expr"
--   ]
--
-- >>> validate (ADT (Grammar [ ... ]))
-- ["Duplicate rule: expr", "Undefined nonterminal: term"]
validate :: ADT -> [String]
validate (ADT (Grammar rules)) =
  let
    (noDupRules, dupNames) = clearDups rules --- duplicate rules removed
    undefNames = findUndef noDupRules
    leftBad    = findLeft noDupRules

    warnDup   = [ "Duplicate rule: "        ++ n | n <- dupNames     ]
    warnUndef = [ "Undefined nonterminal: " ++ n | n <- undefNames   ]
    warnLeft  = [ "Left recursion in: "     ++ n | n <- leftBad      ]
  in
    warnLeft ++ warnUndef ++ warnDup


-- cleanADT:
-- try to automatically remove "bad" rules, so what's left is safe.
--
--
-- >>> cleanADT (ADT (Grammar [Rule "expr" ... , Rule "term" ...]))
-- ADT (Grammar [Rule "expr" ..., Rule "term" ...])
cleanADT :: ADT -> ADT
cleanADT (ADT (Grammar rules)) =
  let
    (startNoDup, _) = clearDups rules

    fixedRules = loopFix clearBad startNoDup

    loopFix f xs =
      let xs' = f xs
      in if length xs' == length xs
            then xs
            else loopFix f xs'
  in
    ADT (Grammar fixedRules)


-- clearBad:
-- after doing this once, some other rules might now become undefined.
-- that's why cleanADT calls loopFix repeatedly.
clearBad :: [Rule] -> [Rule]
clearBad rs =
  let badUndef = findUndef rs
      badUsers = rulesUsing badUndef rs
      badAll   = uniq (badUndef ++ badUsers)
  in
    [ r | r <- rs, ruleName r `notElem` badAll ]


--------------------------------------------------------------------------------
-- helpers for looking at rule sets
--------------------------------------------------------------------------------

-- extraRules:
-- just collect all rule names.
--
-- >>> extraRules [Rule "expr" [] (OptionSet []), Rule "term" [] (OptionSet [])]
-- ["expr","term"]
extraRules :: [Rule] -> [String]
extraRules = map ruleName


-- clearDups:
-- keep only the first definition of each rule name.
-- also return a list of all the names we *skipped* after the first,
-- so we can warn "Duplicate rule: X".
--
-- >>> clearDups [Rule "a" [] x, Rule "a" [] y]
-- ([Rule "a" [] x], ["a"])
clearDups :: [Rule] -> ([Rule],[String])
clearDups = go [] [] []
  where
    go kept dups seen [] = (reverse kept, reverse dups)
    go kept dups seen (r:rs)
      | ruleName r `elem` seen
          = go kept (ruleName r : dups) seen rs
      | otherwise
          = go (r:kept) dups (ruleName r : seen) rs


--------------------------------------------------------------------------------
-- undefined nonterminals
--------------------------------------------------------------------------------

-- findUndef:
-- which nonterminals are *used* on RHS but never *defined* on LHS?
--
-- Steps:
--   def = all rule names actually defined
--   use = all rule names referenced anywhere in bodies
--   result = use - def
--
-- >>> findUndef [Rule "expr" [] (OptionSet [ParticleGroup [PNonTerm "term" Nothing]])]
-- ["term"]
findUndef :: [Rule] -> [String]
findUndef rs =
  let def = extraRules rs
      use = concatMap collectRuleNT rs
  in uniq [ u | u <- use, u `notElem` def ]


-- findLeft:
-- detect direct or indirect left recursion.
-- >>> findLeft [Rule "expr" [] (OptionSet [ParticleGroup [PNonTerm "expr" Nothing]])]
-- ["expr"]
findLeft :: [Rule] -> [String]
findLeft rs =
  [ a
  | a <- extraRules rs
  , a `elem` drop 1 (reach a) 
  ]
  where
    edges =
      [ (n,b)
      | Rule n _ (OptionSet gs) <- rs
      , g <- gs
      , Just b <- firstNT g
      ]

    nextFrom x = [ b | (src,b) <- edges, src == x ]
    reach start = walk [] [start]
      where
        walk seen []     = seen
        walk seen (y:ys)
          | y `elem` seen = walk seen ys
          | otherwise     = walk (y:seen) (nextFrom y ++ ys)


-- firstNT:
-- Look at just the FIRST particle in that branch, and if
-- it's a nonterminal,
-- return its name.
--
-- >>> firstNT (ParticleGroup [PNonTerm "expr" Nothing])
-- [Just "expr"]
firstNT :: ParticleGroup -> [Maybe String]
firstNT (ParticleGroup [])    = [Nothing]
firstNT (ParticleGroup (p:_)) = [pick p]
  where
    pick (PTok q)       = pick q
    pick (PNonTerm n _) = Just n
    pick (PStar q)      = pick q
    pick (PPlus q)      = pick q
    pick (POpt  q)      = pick q
    pick _              = Nothing


-- rulesUsing:
-- Rules are now also considered bad, because they depend on
-- something that doesn't exist.
--
-- >>> rulesUsing ["term"]
--     [Rule "expr" [] (OptionSet [ParticleGroup [PNonTerm "term" Nothing]])]
-- ["expr"]
rulesUsing :: [String] -> [Rule] -> [String]
rulesUsing badNames rs =
  [ ruleName r
  | r <- rs
  , any (`elem` badNames) (collectRuleNT r)
  ]

-- collectRuleNT:
-- walk an entire rule body and list all nonterminals mentioned anywhere.
collectRuleNT :: Rule -> [String]
collectRuleNT (Rule _ _ (OptionSet gs)) =
  concatMap collectGroupNT gs

-- collectGroupNT:
-- same thing but for one branch.
collectGroupNT :: ParticleGroup -> [String]
collectGroupNT (ParticleGroup ps) =
  concatMap collectPartNT ps

-- collectPartNT:
-- dig down into nested stuff like PTok, PStar, PPlus, POpt, PNonTerm ...
collectPartNT :: Particle -> [String]
collectPartNT (PNonTerm n mps) =
  n : maybe [] (concatMap collectPartNT) mps
collectPartNT (PTok p)         = collectPartNT p
collectPartNT (PStar p)        = collectPartNT p
collectPartNT (PPlus p)        = collectPartNT p
collectPartNT (POpt  p)        = collectPartNT p
collectPartNT _                = []


-- uniq:
-- drop duplicates but keep the last occurrence order reversed.
-- basically a simple "unique" for [String] etc.
uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq (x:xs) =
  if x `elem` xs
     then uniq xs
     else x : uniq xs
