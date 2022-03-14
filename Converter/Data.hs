module Converter.Data where

import Data.List (concat, intercalate, intersperse, head, tail)
import Data.Char (isUpper, isLower)

-- Type synonyms for distinguishing terminal and non-terminal symbols
type NTerm = String
type Term = String
type Symbol = String

type RuleType1 = TRule
type RuleType2 = TRule

-- Data type for CFG in the form of G = (N,T,P,S)
data TGrammar = TG {
    nonTerminals :: [NTerm],
    terminals :: [Term],
    startNT :: NTerm,
    rules :: [TRule]
}

instance Show TGrammar where
    show (TG nt t startNT rules) = unlines $ [showNT,showT, startNT] ++ showRules
        where
            showNT = intercalate "," $ map showSymbol nt
            showT = intercalate "," t
            showRules = map show rules

data TMPGrammar = TMPG {
    nonTerminalsTMP :: [NTerm],
    terminalsTMP :: [Term],
    startNTTMP :: NTerm,
    rulesTMP :: [TMPRule]
}

-- Data type for rules of the CFG in the form of N -> (N U Sigma)*
data TRule = TR {
    fromNT :: NTerm,
    toString :: [Symbol]
    } deriving(Eq)

instance Show TRule where
    show (TR nt ts) = showNT ++ ['-','>'] ++ showToString
        where
            showNT = showSymbol nt 
            showToString = concat $ map (showSymbol) ts

-- Function shows NT that are represented by multiple characters
-- as <NT> and NT otherwise
showSymbol :: [Char] -> [Char]
showSymbol [x] = [x]
showSymbol [x,'\''] = [x,'\'']
showSymbol x = "<"++x++">"

-- Couldnt figure out how to use Text.Parsec to parse right side of the rule
-- as a list of list of characters ABCD => [[A],[B],[C],[D]]
-- Resorted to using TMPRule and TMPGrammar and converting after parsing
data TMPRule = TMPR {
    fromNTTMP :: NTerm,
    toStringTMP :: [Char]
    }

-- Predicates and other helpful functions
------------------------------------------------------------------------------------------------------

-- Predicate returns true if the symbol contains only a single upperCase character
isSymbolUpper :: [Symbol] -> Bool
isSymbolUpper (x:xs) = if xs == [] && isCharUpper x then True else False
isSymbolUpper [] = False

-- Predicate returns true if the symbol contains only a single lowerCase character
isSymbolLower :: [Symbol] -> Bool
isSymbolLower (x:xs) = if xs == [] && isCharLower x then True else False
isSymbolLower [] = False

-- Predicate returns true if the symbol contains only a single character
isSingleSymbol :: [Symbol] -> Bool
isSingleSymbol x = if isSymbolUpper x || isSymbolLower x then True else False

-- Predicate returns true if the string contains only a single upperCase character
isCharUpper :: [Char] -> Bool
isCharUpper (x:xs) = if xs == [] && isUpper x then True else False
isCharUpper [] = False

-- Predicate returns true if the string contains only a single upperCase character
isCharLower :: [Char] -> Bool
isCharLower (x:xs) = if xs == [] && isLower x then True else False
isCharLower [] = False

-- Predicate returns true if the string contains only a single character
isChar :: [Char] -> Bool
isChar (x:xs) = if xs == [] then True else False
isChar [] = False

-- Predicate returns true if rule is a simple rule
isSimpleRule :: TRule -> Bool
isSimpleRule rule = if isSymbolUpper (toString rule) then True else False

-- Predicate return true if the nonterminal nt is on the left side of the rule
isFromNTOfRule :: NTerm -> TRule -> Bool
isFromNTOfRule nt rule = if nt == (fromNT rule) then True else False

-- Predicate returns true if the rule is in the form of
-- N -> T
isRuleType1 :: TRule -> Bool
isRuleType1 rule = if isSymbolLower (toString rule) then True else False

-- Predicate returns true if the rule is in the form of
-- N -> NN
isRuleType2 :: TRule -> Bool
isRuleType2 rule = if isCharUpper (head rightSide) && isSymbolUpper (tail rightSide) then True else False
    where rightSide = toString rule

-- Predicate returns true if the rule is in CNF
isRuleInCNF :: TRule -> Bool
isRuleInCNF rule = isRuleType1 rule || isRuleType2 rule

