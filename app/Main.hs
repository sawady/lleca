module Main where

import Lexer
import Parser
import Generator
import qualified Data.Map.Strict as Map

import System.Environment        

main :: IO ()
main = do
    args <- getArgs
    grammar <- readFile (args !! 0)
    -- let tokenized = lexer llecaKeywords llecaSymbols grammar
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "Tokenized grammar-----"
    -- -- mapM_ print tokenized
    -- let parsedGrammar    = parse tokenized
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "parsedGrammar --------"
    -- -- mapM_ print parsedGrammar
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "Reserved symbols in grammar"
    -- let (kws, syms) = calcKeywords parsedGrammar
    -- -- print kws
    -- -- print syms  
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "Initial symbol--------"
    -- -- print (initialMetaSymbol parsedGrammar)
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "Meta symbols----------"
    -- let metas = metaSymbols parsedGrammar
    -- -- mapM_ print metas
    -- -- putStrLn "----------------------"
    -- -- putStrLn "----------------------"
    -- -- putStrLn "Terminal symbols------"
    -- let terminals = terminalSymbols parsedGrammar
    -- mapM_ print terminals
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "First of grammar ------"
    -- let fs = firstSet terminals metas parsedGrammar
    -- mapM_ print $ Map.assocs fs
    -- mapM_ print terminals
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Follow of grammar -----"
    -- let ffs = followSet metas parsedGrammar fs
    -- mapM_ print $ Map.assocs ffs
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Table of grammar -----"
    -- mapM_ print $ Map.assocs $ generateTable terminals metas parsedGrammar fs ffs
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Tokenized input file--"
    input <- readFile (args !! 1)
    -- mapM_ print (lexer kws syms input)
    t <- parseTermino grammar input
    print t

