
module Converter.Parse where

import Control.Monad
import Control.Arrow
import Control.Applicative

import Data.List (nub)

import Text.Parsec
import Text.Parsec.String (Parser)

import Converter.Data

type Error = Either String

-- Function parses input string into data TGrammar and checks if it is valid (+ some conversions form TMPG)
parseTG :: String -> Error TGrammar
parseTG = checkTG <=< convertTMPGtoTG . (left show) . parse parserTG ""

-- Crude solution to parsing ABCD as [[A],[B],[C],[D]] and not as [A,B,C,D].
-- Uses TMPG to first parse the rightSide of the rules as strings and then
-- converts them to list of single character strings
convertTMPGtoTG :: Error TMPGrammar -> Error TGrammar
convertTMPGtoTG (Right tmpg@(TMPG nt t snt rules)) =Right (TG nt t snt (convertTMPRules rules))
convertTMPGtoTG (Left x) = Left x

convertTMPRules :: [TMPRule] -> [TRule]
convertTMPRules rules = map (convertRule) rules
    where convertRule r@(TMPR fromNT toString) = (TR fromNT (divvyUp toString))

-- Function splits string into a list of single character strings
-- Same as divvy 1 1 x ?
-- [A,B,C,D] => [[A],[B],[C],[D]]
divvyUp :: String -> [String]
divvyUp input = map (:[]) input

-- Parse input
----------------------------------------------------------------------------------------------------

-- Parse TGrammar
-- G = (N,T,P,S)
parserTG :: Parser TMPGrammar
parserTG = TMPG <$> nonTermsParse <* newline
              <*> termsParse    <* newline
              <*> startNTParse  <* newline
              <*> rulesParse 

-- Parse a list of nonterminal characters
-- [N] = N | N \in [A-Z]
nonTermsParse :: Parser [NTerm]
nonTermsParse = sepBy1 nonTerm (char ',')

nonTerm :: Parser NTerm
nonTerm = many1 upper

-- Parse a list of terminal characters
-- [T] = T | T \in [a-z]
termsParse :: Parser [Term]
termsParse = sepBy1 term (char ',')

term :: Parser Term
term = many1 lower

-- Parse the starting non terminal character
-- S \in N
startNTParse :: Parser NTerm
startNTParse = many1 upper

-- Parse a list of rules separated by "\n"
-- rule \n rule \n ...
rulesParse :: Parser [TMPRule]
rulesParse = endBy ruleParse newline

-- Parse a single rule
-- N -> (N U T)*
ruleParse :: Parser TMPRule
ruleParse = TMPR <$> nonTerm <* string "->" <*> rightSide

rightSide :: Parser [Char]
rightSide = many1 letter

-- Check Validity
----------------------------------------------------------------------------------------------

-- Check if the input TGrammar follows the requisite rules
-- 1. Symbols in rules are from (N U T)
-- 2. Starting symbol is in N
checkTG :: TGrammar -> Error TGrammar
checkTG tg@(TG nt t snt rules) = if condition then Right tg else Left "Error: Invalid Grammar"
    where condition = (checkSymbols nt) && (checkSymbols t) && (checkRules nt t rules) && (checkSNT nt snt)

-- Predicate that returns true if there are no duplicates in a list x
-- Used for checking unique N and T characters
checkSymbols :: (Ord a) => [a] -> Bool
checkSymbols x = length (nub x) == length x

-- Predicate that returns true if every rule in the list of rules
-- has fromNT in N and toString in (N U T)*
checkRules :: [NTerm] -> [Term] -> [TRule] -> Bool
checkRules nt t (x:xs) = elem (fromNT x) nt && checkRightSide nt t (toString x) && checkRules nt t xs 
checkRules nt t [] = True

checkRightSide :: [NTerm] -> [Term] -> [Symbol] -> Bool
checkRightSide nt t rightSide = all (checkToString nt t) rightSide

-- Predicate that returns true if a character from toString is in N or T
checkToString :: [NTerm] -> [Term] -> Symbol -> Bool
checkToString nt t x = elem x nt || elem x t

-- Predicate that returns true if starting non terminal is in N
checkSNT :: [NTerm] -> NTerm -> Bool
checkSNT nt snt = elem snt nt

