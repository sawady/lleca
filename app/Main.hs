module Main where

import Lexer
import Parser
import Data.Char
import Data.List
import qualified Data.Set as Set

llecaKeywords = ["_", "ID", "STRING", "NUM"]
llecaSymbols  = ["|", "=>", "$", "(", ")", ",", "[", "]"]

calcKeywords :: Grammar -> ([String], [String])
calcKeywords g = partition isVar (concat $ map collect g)
    where collect (Production _ syms _) = map get $ filter isString syms
          get (SString s) = s
          isString (SString s) = True
          isString _           = False
          isVar xs = head xs == '_' || isLetter (head xs)

main :: IO ()
main = do
    contents <- getContents
    let tokenized = lexer llecaKeywords llecaSymbols contents
    let parsed    = parse tokenized
    mapM print parsed
    putStrLn "----------------------"
    putStrLn "----------------------"
    putStrLn "Reserved symbols------"
    print $ calcKeywords parsed