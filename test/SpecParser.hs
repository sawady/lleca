module SpecParser where

import Test.Hspec
import System.Environment

import Lexer
import Parser

test :: IO ()
test = do
    grammar <- readFile "test/input/expresiones/gramatica.ll"
    let tokenized = lexer llecaKeywords llecaSymbols grammar
    let parsedGrammar = parse tokenized
    let (kws, syms) = calcKeywords parsedGrammar
    let terminals = terminalSymbols parsedGrammar
    let metas = metaSymbols parsedGrammar
    let initialMetaSymbol = head metas
    hspec $ do
        describe "reserved keywords" $ do
           it "keywords of expressions grammar" $ do
               kws `shouldBe` ["div","mod"]
               syms `shouldBe` ["(",")","+","-","*"]
        describe "symbols of expressions grammar" $ do
            it "initial meta symbol" $ initialMetaSymbol `shouldBe` (SymMeta "expresion")
            it "terminal symbols" $ all (`elem` terminals) [SymNUM, SymLit "(", SymLit ")", SymLit "+", SymLit "-", SymLit "*", SymLit "div", SymLit "mod"]
            it "meta symbols" $ all (`elem` metas) [SymMeta "expresion", SymMeta "factor", SymMeta "expresion1", SymMeta "termino", SymMeta "termino1"]