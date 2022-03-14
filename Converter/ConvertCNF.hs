
module Converter.ConvertCNF where

import Data.List
import Debug.Trace


import Converter.Data

convertCFG :: TGrammar -> TGrammar
convertCFG tg = addNewNonterminals $ addNewRules tg

addNewNonterminals :: TGrammar -> TGrammar
addNewNonterminals tg@(TG nt t snt rules ) = (TG getNTS t snt rules)
    where getNTS = nub $ nt ++ map (fromNT) rules

addNewRules :: TGrammar -> TGrammar
addNewRules tg@(TG nt t snt rules) = (TG nt t snt (getRulesCNF rules))

-- Function returns new rules for CFG that are in CNF
-- It keeps rules that are already in CNF and applies
-- conversion algorithm to the rest
getRulesCNF :: [TRule] -> [TRule]
getRulesCNF x = rulesAlreadyInCNF x ++ (nub $ convertRest x)
    where 
        rulesAlreadyInCNF rules = filter (isRuleInCNF) rules
        convertRest rules = convertRulesToCNF $ filter (not . isRuleInCNF) rules


-- Function converts rules that are not in CNF into CNF
convertRulesToCNF :: [TRule] -> [TRule]
convertRulesToCNF (rule:xs) = convertRuleToCNF rule ++ convertRulesToCNF xs
convertRulesToCNF [] = []

{-isRuleInCNFTrace rule = trace ("Rule : " ++ show rule ++" IsInCNF: "++ show result ++"\n") result
    where result = isRuleInCNF rule-}

-- Function converts a single rule to CNF by repeatedly splitting
-- the right side of the rule into two nonterminals
convertRuleToCNF :: TRule -> [TRule]
convertRuleToCNF r@(TR fromNT toString) = 
    if isRuleInCNF r then
        step1_2 r
    else if length toString == 2 then
        step5 r
    else 
        newRule ++ maybeRuleForFirstNT (firstNonTerminal toString) ++ convertRuleToCNF ruleWithNewNT
    where
        newRule = [(TR fromNT [firstNonTerminal toString, secondNonTerminal toString])]
        ruleWithNewNT = (TR (secondNonTerminal toString) (tail toString))

-- Function creates a new nonterminal form the first symbol
-- of the right side of the rule
-- ABCD -> A | aBCD -> a'
firstNonTerminal :: [Symbol] -> Symbol
firstNonTerminal toString = 
    if isCharLower firstChar then
        firstChar ++ "'"
    else
        firstChar
    where
        firstChar = head toString

-- Function merges the right side of the rule into a single nonterminal
-- excluding the first symbol
-- ABCD -> <BCD>
secondNonTerminal :: [Symbol] -> Symbol
secondNonTerminal toString = concat $ tail toString

-- Function returns new rule to create terminal symbol from
-- new nonterminal symbol T' if necessary
-- T'-> T | Empty
maybeRuleForFirstNT :: Symbol -> [TRule]
maybeRuleForFirstNT nt = if length nt == 1 then [] else step6 [head nt]

-- Function for rules that are already in CNF
-- N -> T
-- N -> NN
step1_2 :: TRule -> [TRule]
step1_2 rule = [rule]

-- Function converts a rule with Term and NTerm to CNF
-- N -> TN | N -> NT | N -> TT
step5 :: TRule -> [TRule]
step5 r@(TR fromNT toString) = 
    if areTermBoth then
        [(TR fromNT (getToStringBoth toString))] ++ step6 (head toString) ++ step6 (concat $ tail toString)
    else if isTermFirst then
        [(TR fromNT (getToStringLeft toString))] ++ step6 (head toString)
    else
        [(TR fromNT (getToStringRight toString))] ++ step6 (concat $ tail toString)
    where 
        areTermBoth = isTermFirst && isTermSecond
        isTermFirst = isCharLower $ head toString
        isTermSecond = isCharLower $ (concat $ tail toString)
        getToStringLeft (x:xs) = [(x ++ "'"), head xs]
        getToStringRight (x:xs) = [x,((head xs)++"'")]
        getToStringBoth (x:xs) = [(x ++ "'"),((head xs)++"'")] 

{-step5Trace rule = trace ("Rule :" ++ show rule ++ " Result :" ++ show result) result
    where result = step5 rule-}

-- Function adds new rules for a new nonterminal symbol T'
-- T' -> T
step6 :: Term -> [TRule]
step6 t = [(TR (t++"'") [t])]


