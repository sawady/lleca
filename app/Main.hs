module Main where

import Lexer
import Parser
import Data.Char
import Data.List

llecaKeywords = ["_", "ID", "STRING", "NUM"]
llecaSymbols  = ["|", "=>", "$", "(", ")", ",", "[", "]"]

calcKeywords :: Grammar -> ([String], [String])
calcKeywords g = partition isVar (concat $ map collect1 g)
    where collect1 (Rule name prods) = concat $ map collect2 prods
          collect2 (Production syms _) = map get $ filter isString syms
          get (SString s) = s
          isString (SString s) = True
          isString _           = False
          isVar xs = head xs == '_' || isLetter (head xs)

main :: IO ()
main = do
    contents <- getContents
    let tokenized = lexer llecaKeywords llecaSymbols contents
    let parsed    = parse tokenized
    print parsed
    putStrLn "----------------------"
    putStrLn "----------------------"
    putStrLn "----------------------"
    print $ calcKeywords parsed