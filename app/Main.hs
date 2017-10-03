{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad(when)
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import Lexer
import Parser
import Generator
import qualified Data.Map.Strict as Map

data LlecaArgs = LlecaArgs {
    tokens :: Bool,
    productions :: Bool,
    first :: Bool,
    follow :: Bool,
    table :: Bool,
    grammar :: String,
    file :: String
 } deriving (Show, Data, Typeable)

llecaConsole = LlecaArgs {
    tokens      = def &= help "Output the tokenization result of grammar and input to the console" &= name "t",
    productions = def &= help "Output the Grammar productions result to the console" &= name "p",
    first       = def &= help "Output the first set",
    follow      = def &= help "Output the follow set",
    table       = def &= help "Output LL(1) table of grammar",
    grammar     = def &= help "The grammar file" &= typ "[input]" &= name "g",
    file        = def &= help "The input file" &= typ "[input]" &= name "f"
 }

main :: IO ()
main = do
    args <- (cmdArgs $ llecaConsole
                     &= help "The Lleca parser generator"
                     &= program "Lleca"
                     &= summary "Lleca v1.0.0.0"
            )

    dir <- getCurrentDirectory

    grammarFile <- (canonicalizePath (grammar args))
    existsGrammar <- doesFileExist (grammarFile)
    when (not existsGrammar) (die ("The given grammar file " ++ (file args) ++ " does not exist"))

    inputFile <- (canonicalizePath (file args))
    when (null inputFile)  (die "An program input file is mandatory")
    existsInput <- doesFileExist (inputFile)
    when (not existsInput) (die ("The given input file " ++ (file args) ++ " does not exist"))

    grammarContents <- readFile grammarFile
    inputContents <- readFile inputFile

    let tokenizedGrammar = lexer llecaKeywords llecaSymbols grammarContents
    when (tokens args) $ do
        putStrLn "Tokenized grammar:"
        mapM_ print tokenizedGrammar
        putStrLn ""
        
    let parsedGrammar = parse tokenizedGrammar
    let (kws, syms) = calcKeywords parsedGrammar
    let terminals = terminalSymbols parsedGrammar
    let metas = metaSymbols parsedGrammar
    when (productions args) $ do
        putStrLn "grammar productions:"
        mapM_ print parsedGrammar
        putStrLn ""
        putStrLn "initial metasymbol (non terminal):"
        print $ initialMetaSymbol parsedGrammar
        putStrLn ""
        putStrLn "metasymbols (non terminal):"
        print $ metas
        putStrLn ""
        putStrLn "Terminal symbols:"
        print $ terminals
        putStrLn ""
        putStrLn "Reserved symbols:"
        print kws
        print syms
        putStrLn ""        

    let fs = firstSet terminals metas parsedGrammar
    when (first args) $ do
        putStrLn "First set of grammar:"
        mapM_ print $ Map.assocs fs
        putStrLn ""

    let ffs = followSet metas parsedGrammar fs
    when (follow args) $ do
        putStrLn "Follow set of grammar:"
        mapM_ print $ Map.assocs ffs
        putStrLn ""

    let ll1table = generateTable terminals metas parsedGrammar fs ffs
    when(table args) $ do
        putStrLn "Table of grammar"
        mapM_ print $ Map.assocs $ ll1table
        putStrLn ""

    let tokenizedInput = lexer kws syms inputContents

    when (tokens args) $ do
        putStrLn "Tokenized input program:"
        mapM_ print tokenizedInput
        putStrLn ""

    let firstSymbol = head metas
    
    putStrLn "Generated AST of input file based on grammar:"
    print $ analize firstSymbol ll1table (tokenizedInput ++ [TokenDollar])
    
    return ()
