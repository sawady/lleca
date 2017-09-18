module Main where

import Lexer

main :: IO ()
main = do
    s <- getContents
    print s
    putStrLn "------------"
    putStrLn "------------"
    putStrLn "------------"
    print (lexer ["fun"] ["{", "}", ":", "(", "*", "+", ")", ",", ":=", "-", "==", ">=", "<=", ">", "<", "!=", "[", "]", "#", "|", "=", "$", "=>", "\\", "."] s)