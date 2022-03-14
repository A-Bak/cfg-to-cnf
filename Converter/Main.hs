
module Main (main)  where

import Control.Monad

import System.Environment
import System.Exit
import System.IO

import Converter.Data 
import Converter.Parse
import Converter.SimpleRules (removeSimpleRules)
import Converter.ConvertCNF (convertCFG)

main :: IO ()
main = do
    (command, input) <- getCommand =<< getArgs
    either error command (parseTG input)

-- Function takes arguments from getArgs and returns Command + Input
-- Command is one of the options -i, -1, -2:
-- -i -> Checks and prints CFG
-- -1 -> Removes simple rules and prints CFG
-- -2 -> Transforms CFG to CNF and prints CFG
getCommand :: [String] -> IO (TGrammar -> IO (), String)
getCommand [command,file] = do
    input <- readFile file
    case command of
        "-i" -> return (printTGrammar,input)
        "-1" -> return (noSimpleRules,input)
        "-2" -> return (convertCFG2CNF,input)
        _ -> error ("Error: Invalid argument "++ command)
        
getCommand [command] = do
    input <- getContents
    case command of
        "-i" -> return (printTGrammar,input)
        "-1" -> return (noSimpleRules,input)
        "-2" -> return (convertCFG2CNF,input)
        _ -> error ("Error: Invalid argument "++ command)

getCommand [] = invalidArguments
getCommand _  = invalidArguments

-- Function that exits with error message when arguments are invalid
invalidArguments :: IO (TGrammar -> IO (), String)
invalidArguments = error "Error: Invalid Arguments \n To run the program input ./cfg-2-cnf [-i|-1|-2] [File|_] \n If the FILE is left blank it is necessary to input grammar from stdin"

-- Function prints TGrammar to stdout
printTGrammar :: TGrammar -> IO ()
printTGrammar tg = do
    putStr (show tg)

-- Function removes simple rules from CFG
noSimpleRules :: TGrammar -> IO ()
noSimpleRules tg = do 
    let g = removeSimpleRules tg
    printTGrammar g

-- Function converts CFG to Chomsky normal form
convertCFG2CNF :: TGrammar -> IO ()
convertCFG2CNF tg = do
    let g = convertCFG $ removeSimpleRules tg
    printTGrammar g







    



