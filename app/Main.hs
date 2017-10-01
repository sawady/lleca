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
    let tokenized = lexer llecaKeywords llecaSymbols grammar
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Tokenized grammar-----"
    -- mapM_ print tokenized
    let parsedGrammar    = parse tokenized
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "parsedGrammar --------"
    -- mapM_ print parsedGrammar
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Reserved symbols in grammar"
    let (kws, syms) = calcKeywords parsedGrammar
    -- print kws
    -- print syms  
    input <- readFile (args !! 1)
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Tokenized input file--"
    -- mapM_ print (lexer kws syms input)
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Initial symbol--------"
    -- print (initialMetaSymbol parsedGrammar)
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Meta symbols----------"
    let metas = metaSymbols parsedGrammar
    -- mapM_ print metas
    -- putStrLn "----------------------"
    -- putStrLn "----------------------"
    -- putStrLn "Terminal symbols------"
    let terminals = terminalSymbols parsedGrammar
    mapM_ print terminals
    putStrLn "----------------------"
    putStrLn "----------------------"
    putStrLn "First of grammar ------"
    let fs = firstSet terminals metas parsedGrammar
    mapM_ print $ Map.assocs fs
    mapM_ print terminals
    putStrLn "----------------------"
    putStrLn "----------------------"
    putStrLn "Follow of grammar -----"
    mapM_ print $ Map.assocs $ followSet metas parsedGrammar fs

