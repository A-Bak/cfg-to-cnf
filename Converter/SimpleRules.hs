
module Converter.SimpleRules where

import Debug.Trace

import Data.Char
import Data.List

import Converter.Data

-- Function removes rules in the form of N -> N from TGrammar 
-- G = (N,T,P,S) and returns changed TGrammar G'= (N,T,P',S)
removeSimpleRules :: TGrammar -> TGrammar
removeSimpleRules (TG nt t snt rules) = (TG nt t snt (newRules nt (getNoSimpleRules rules) (getSimpleRules rules)))

-- Finding a group of reachable nonterminals (N,[N]) by only using simple rules from each NT
-- Adding necessary rules for each group (N,[N]) so that simple rules can be removed
-------------------------------------------------------------------------------------------

-- Funtion returns new rules created by removing simple rules
newRules :: [NTerm] -> [TRule] -> [TRule] -> [TRule]
newRules nts noSimpleRules simpleRules = concat $ map (getRulesFromGroup noSimpleRules) $ map (getReachableForNT simpleRules) nts
newRules [] _ _ = []

-- Function returns a list of rules which have one of the nonterminals fromNT
-- the goup of nonterminals on the left side
getRulesFromGroup :: [TRule] -> (NTerm,[NTerm]) -> [TRule]
getRulesFromGroup noSimpleRules (nt,group) = map (replaceNTInRule nt) $ concat $ map (getRulesForNT noSimpleRules) group

-- Function finds a group of non terminals reachable from nt by using simple rules
getReachableForNT :: [TRule] -> NTerm -> (NTerm,[NTerm])
getReachableForNT rules nt = (nt,(createGroupOfNT [nt] rules))

-- Function returns a group of nonterminals reachable from N by only simple rules
-- B \in N | A ->* B
createGroupOfNT :: [NTerm] -> [TRule] -> [NTerm]
createGroupOfNT nts rules = if nts /= (nextStepCGNT nts rules) then createGroupOfNT (nextStepCGNT nts rules) rules else nts

-- Function concatenates all of the applications of simple rules to group of nonterminals nts
nextStepCGNT :: [NTerm] -> [TRule] -> [NTerm]
nextStepCGNT nts rules = removeDuplicateNT (foldl (++) nts (map (applySimpleRules rules) nts))

-- Function applies all of the simple rules to given nonterminal
applySimpleRules :: [TRule] -> NTerm -> [NTerm]
applySimpleRules rules nt = map (applySimpleRule nt) rules
--applySimpleRules [] result nt = result

-- Function returns right side of the simple rule if nt is on left side
-- A -> B = B | A == nt
applySimpleRule ::  NTerm -> TRule -> NTerm 
applySimpleRule nt rule = if nt == (fromNT rule) && isSimpleRule rule then head (toString rule) else []

-- Funtion removes duplicate nonterminals from the list
removeDuplicateNT :: [NTerm] -> [NTerm]
removeDuplicateNT nts = nub nts

-- Finding simple rules
-------------------------------------------------------------------------------------------

-- Function returns rules which have a single nonterminal on the right side
-- N -> N
getSimpleRules :: [TRule] -> [TRule]
getSimpleRules rules = filter (isSymbolUpper . toString) (getRulesWithOneChar rules)

-- Function returns rules which have a single character on the right side
-- N -> (N U T)
getRulesWithOneChar :: [TRule] -> [TRule]
getRulesWithOneChar rules = filter (isSingleSymbol . toString) rules

-- Function returns list of rules without any simple rules
getNoSimpleRules :: [TRule] -> [TRule]
getNoSimpleRules rules = filter (`notElem` (getSimpleRules rules)) rules

-- Function returns all rules which have nt on the left side
-- A \in N | nt == A
getRulesForNT :: [TRule] -> NTerm -> [TRule]
getRulesForNT rules nt = filter (isFromNTOfRule nt) rules

-- Function replaces left side of the rule by nonterminal nt
-- A -> Alpha = B -> Alpha
replaceNTInRule :: NTerm -> TRule -> TRule
replaceNTInRule nt rule@(TR fromNT toString) = (TR nt toString) 







