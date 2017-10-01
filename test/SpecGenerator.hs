module SpecGenerator where

import Test.Hspec
import System.Environment

import Control.Monad
import Generator
import Lexer
import Parser

test :: IO ()
test = do
    (grammarExp, inputsExp) <- testExpresiones
    (grammarLam, inputsLam) <- testLambda
    (grammarLleca, inputsLleca) <- testLleca
    (grammarCucaracha, inputsCucaracha) <- testCucaracha
    hspec $ do
        describe "Expresiones" $ do 
            mapM_ (check grammarExp) inputsExp
        describe "Lambda" $ do 
            mapM_ (check grammarLam) inputsLam
        describe "Lleca" $ do 
            mapM_ (check grammarLleca) inputsLleca
        describe "Cucaracha" $ do 
            mapM_ (check grammarCucaracha) inputsCucaracha

check grammar (name, (input, expected)) =  
    do 
       let t = parseTermino grammar input
       it name $ show t `shouldBe` expected

testExpresiones = testGrammar "test/input/expresiones/gramatica.ll"
                              ["test/input/expresiones/test00.input", "test/input/expresiones/test01.input" ]
                              ["test/input/expresiones/test00.expected", "test/input/expresiones/test01.expected" ]

testLambda = testGrammar "test/input/lambda/gramatica.ll"
                         ["test/input/lambda/test00.input", "test/input/lambda/test01.input" ]
                         ["test/input/lambda/test00.expected", "test/input/lambda/test01.expected" ]

testLleca = testGrammar "test/input/lleca_en_lleca/gramatica.ll"
                        (map (\x -> "test/input/lleca_en_lleca/test0" ++ x ++ ".input") (map show [0..3]))
                        (map (\x -> "test/input/lleca_en_lleca/test0" ++ x ++ ".expected") (map show [0..3]))

testCucaracha = testGrammar "test/input/cucaracha/gramatica.ll"
                    (map (\x -> "test/input/cucaracha/test" ++ x ++ ".input") (map (\x -> "0" ++ show x) [0..9] ++ map show [10..55]))
                    (map (\x -> "test/input/cucaracha/test" ++ x ++ ".expected") (map (\x -> "0" ++ show x) [0..9] ++ map show [10..55]))


testGrammar :: String -> [String] -> [String] -> IO (String, [(String, (String, String))])
testGrammar grammarFile inputFiles expectedFiles = do
    grammar <- readFile grammarFile
    inputs <- mapM readFile inputFiles
    expected <- mapM readFile expectedFiles
    return (grammar, map (\(n, p) -> ("test: " ++ n, p)) $ zip (map show [0..]) $ zip inputs expected)
