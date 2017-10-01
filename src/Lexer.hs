module Lexer where

import Data.Char
import Data.List
import Data.Maybe

data Token = TokenId String
           | TokenNum Int
           | TokenString String
           | TokenLit String 
           | TokenDollar deriving (Show, Eq)

lexer :: [String] -> [String] -> String -> [Token]
lexer kws syms [] = []
lexer kws syms ('/':'*':cs) = deleteComment kws syms cs
lexer kws syms (c:cs)
      | isSpace c = lexer kws syms cs
      | isDigit c = lexNum kws syms (c:cs)      
      | isAlpha c || c == '_' = lexLitOrId kws syms (c:cs)
      | c == '\"' = lexString kws syms cs
      | otherwise = lexSymbol kws syms (c:cs)

deleteComment kws syms [] = [] 
deleteComment kws syms ('*':'/':cs) = lexer kws syms cs 
deleteComment kws syms (c:cs) = deleteComment kws syms cs 

lexNum kws syms cs = TokenNum (read num) : lexer kws syms rest
    where (num,rest) = span isDigit cs

lexLitOrId kws syms cs =
    (if elem s kws
        then TokenLit s
        else TokenId s) : lexer kws syms rest

    where (s, rest) = span (\x -> isAlphaNum x || x == '_') cs

lexString kws syms cs = TokenString s : lexer kws syms (tail rest)
    where (s, rest) = span (\x -> x /= '\"') (desescape cs)
          desescape []             = []
          desescape ('\\':'\\':xs) = '\\' : desescape xs
          desescape ('\\':'\"':xs) = '\"' : desescape xs
          desescape (x:xs)         = x : desescape xs

lexSymbol kws syms cs = 
    case s of 
        Nothing  -> error $ "Error de sintaxis: caracter desconocido en la entrada " ++ [head cs]
        Just sym -> TokenLit sym : lexer kws syms stripped

    where s = find (\x -> isPrefixOf x cs) orderedSyms
          stripped = fromJust (stripPrefix (fromJust s) cs)
          orderedSyms = reverse (sort syms)