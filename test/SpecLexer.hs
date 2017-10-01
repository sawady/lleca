module SpecLexer(test) where

import Test.Hspec

import Lexer

keywords = ["fun"] 
symbols  = ["{", "}", ":", "(", "*", "+", ")", ",", ":=", "-", "=", "==", ">=", "<=", ">", "<", "!=", "[", "]", "#", "|", "$", "=>", "\\", "."]

test :: IO ()
test = hspec $ do
    describe "basic" $ do
        it "equals empty" $ do
            lexer keywords symbols "" `shouldBe` []

        it "divide symbols" $ do
            lexer keywords symbols "+++" `shouldBe` [TokenLit "+", TokenLit "+", TokenLit "+"]

        it "precedence" $ do
            lexer keywords symbols "<====" `shouldBe` [TokenLit "<=", TokenLit "==", TokenLit "="]

        it "comments" $ do
            lexer keywords symbols "hola/*hola*/hola" `shouldBe` [TokenId "hola", TokenId "hola"]

    describe "expresiones" $ do
        it "test00" $ do
            input <- readFile "test/input/expresiones/test00.input"
            lexer keywords symbols input `shouldBe` inputExp00

        it "test01" $ do
            input <- readFile "test/input/expresiones/test00.input"
            lexer keywords symbols input `shouldBe` inputExp00

    describe "lambda" $ do
        it "test00" $ do
            input <- readFile "test/input/lambda/test00.input"
            lexer keywords symbols input `shouldBe` inputLambda00

        it "test01" $ do
            input <- readFile "test/input/lambda/test01.input"
            lexer keywords symbols input `shouldBe` inputLambda01

    describe "cucaracha" $ do
        it "test09" $ do
            input <- readFile "test/input/cucaracha/test09.input"
            lexer keywords symbols input `shouldBe` inputCucaracha09

    describe "lleca_en_lleca" $ do
        it "test00" $ do
            input <- readFile "test/input/lleca_en_lleca/test00.input"
            lexer keywords symbols input `shouldBe` inputLleca00

        it "test01" $ do
            input <- readFile "test/input/lleca_en_lleca/test01.input"
            lexer keywords symbols input `shouldBe` inputLleca01

inputExp00 = [TokenLit "(",TokenNum 1,TokenLit "*",TokenNum 22,TokenLit "+",TokenNum 333,TokenLit "*",TokenNum 4444,TokenLit ")",TokenId "div",TokenLit "(",TokenNum 1,TokenLit "*",TokenLit "(",TokenNum 22,TokenLit "+",TokenNum 333,TokenLit ")",TokenLit ")"]
inputExp01 = [TokenNum 1000,TokenId "mod",TokenNum 3,TokenLit "+",TokenNum 2000,TokenId "mod",TokenNum 5,TokenLit "+",TokenNum 3000,TokenId "mod",TokenNum 7]

inputLambda00 = [TokenLit "\\",TokenId "x",TokenId "y",TokenLit ".",TokenId "x"]
inputLambda01 = [TokenLit "(",TokenLit "\\",TokenId "x",TokenId "y",TokenLit ".",TokenId "y",TokenLit "(",TokenId "x",TokenId "x",TokenId "y",TokenLit ")",TokenLit ")",TokenLit "(",TokenLit "\\",TokenId "x",TokenId "y",TokenLit ".",TokenId "y",TokenLit "(",TokenId "x",TokenId "x",TokenId "y",TokenLit ")",TokenLit ")"]

inputCucaracha09 = [TokenLit "fun",TokenId "main",TokenLit "(",TokenLit ")",TokenLit "{",TokenId "x",TokenLit ":=",TokenNum 104,TokenId "putChar",TokenLit "(",TokenId "x",TokenLit ")",TokenId "x",TokenLit ":=",TokenNum 111,TokenId "putChar",TokenLit "(",TokenId "x",TokenLit ")",TokenId "x",TokenLit ":=",TokenNum 108,TokenId "putChar",TokenLit "(",TokenId "x",TokenLit ")",TokenId "x",TokenLit ":=",TokenNum 97,TokenId "putChar",TokenLit "(",TokenId "x",TokenLit ")",TokenId "x",TokenLit ":=",TokenNum 10,TokenId "putChar",TokenLit "(",TokenId "x",TokenLit ")",TokenLit "}"]

inputLleca00 = [TokenId "expresion",TokenLit "|",TokenId "termino",TokenId "expresion1",TokenLit "=>",TokenLit "$",TokenNum 2,TokenLit "[",TokenLit "$",TokenNum 1,TokenLit "]",TokenId "expresion1",TokenLit "|",TokenLit "=>",TokenId "_",TokenLit "|",TokenString "+",TokenId "termino",TokenId "expresion1",TokenLit "=>",TokenLit "$",TokenNum 3,TokenLit "[",TokenId "add",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenLit "]",TokenLit "|",TokenString "-",TokenId "termino",TokenId "expresion1",TokenLit "=>",TokenLit "$",TokenNum 3,TokenLit "[",TokenId "sub",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenLit "]",TokenId "termino",TokenLit "|",TokenId "factor",TokenId "termino1",TokenLit "=>",TokenLit "$",TokenNum 2,TokenLit "[",TokenLit "$",TokenNum 1,TokenLit "]",TokenId "termino1",TokenLit "|",TokenLit "=>",TokenId "_",TokenLit "|",TokenString "*",TokenId "factor",TokenId "termino1",TokenLit "=>",TokenLit "$",TokenNum 3,TokenLit "[",TokenId "mul",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenLit "]",TokenLit "|",TokenString "div",TokenId "factor",TokenId "termino1",TokenLit "=>",TokenLit "$",TokenNum 3,TokenLit "[",TokenId "div",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenLit "]",TokenLit "|",TokenString "mod",TokenId "factor",TokenId "termino1",TokenLit "=>",TokenLit "$",TokenNum 3,TokenLit "[",TokenId "mod",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenLit "]",TokenId "factor",TokenLit "|",TokenId "NUM",TokenLit "=>",TokenLit "$",TokenNum 1,TokenLit "|",TokenString "(",TokenId "expresion",TokenString ")",TokenLit "=>",TokenLit "$",TokenNum 2]
inputLleca01 = [TokenId "termino",TokenLit "|",TokenId "atomo",TokenId "argumentos",TokenLit "=>",TokenLit "$",TokenNum 2,TokenLit "[",TokenLit "$",TokenNum 1,TokenLit "]",TokenLit "|",TokenString "\\",TokenId "parametros",TokenString ".",TokenId "termino",TokenLit "=>",TokenLit "$",TokenNum 2,TokenLit "[",TokenLit "$",TokenNum 4,TokenLit "]",TokenId "parametros",TokenLit "|",TokenId "ID",TokenId "parametros2",TokenLit "=>",TokenId "Lam",TokenLit "(",TokenLit "$",TokenNum 1,TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenId "parametros2",TokenLit "|",TokenLit "=>",TokenId "_",TokenLit "|",TokenId "ID",TokenId "parametros2",TokenLit "=>",TokenId "Lam",TokenLit "(",TokenLit "$",TokenNum 1,TokenLit ",",TokenLit "$",TokenNum 2,TokenLit ")",TokenId "argumentos",TokenLit "|",TokenLit "=>",TokenId "_",TokenLit "|",TokenId "atomo",TokenId "argumentos",TokenLit "=>",TokenLit "$",TokenNum 2,TokenLit "[",TokenId "App",TokenLit "(",TokenId "_",TokenLit ",",TokenLit "$",TokenNum 1,TokenLit ")",TokenLit "]",TokenId "atomo",TokenLit "|",TokenId "ID",TokenLit "=>",TokenId "Var",TokenLit "(",TokenLit "$",TokenNum 1,TokenLit ")",TokenLit "|",TokenString "(",TokenId "termino",TokenString ")",TokenLit "=>",TokenLit "$",TokenNum 2]