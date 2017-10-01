module Terms where

import Lexer
import Parser
import Text.PrettyPrint

data Termino = 
    Agujero
  | Cadena String
  | Numero Int
  | Estructura String [Termino] deriving Eq

equalSym :: Token -> Symbol -> Bool
equalSym (TokenId s) SymID = True
equalSym (TokenNum n) SymNUM = True
equalSym (TokenString s) SymSTRING = True
equalSym (TokenLit s1) (SymLit s2) = s1 == s2
equalSym TokenDollar DollarSign = True
equalSym _ _ = False

leaf :: Token -> Termino
leaf (TokenId s) = Estructura s []
leaf (TokenNum n) = Numero n
leaf (TokenString s) = Cadena s
leaf (TokenLit s) = Estructura s []

toSym :: Token -> Symbol
toSym (TokenId s) = SymID
toSym (TokenNum n) = SymNUM
toSym (TokenString s) = SymSTRING
toSym (TokenLit s) = SymLit s
toSym TokenDollar = DollarSign

-- SIMPLE VERSION
-- instance Show Termino where
--     show Agujero = "_"
--     show (Cadena string) = "\"" ++ string ++ "\""
--     show (Numero n) = show n
--     show (Estructura n ts) = if null ts
--                                 then n
--                                 else n ++ "(" ++ intercalate ", " (map show ts) ++ ")"

instance Show Termino where
    show p = render $ (termRepr p idnt) $$ text ""

idnt :: Int
idnt = 0

termRepr :: Termino -> Int -> Doc
termRepr Agujero n = text "Agujero"
termRepr (Cadena string) n = text $ "\"" ++ string ++ "\""
termRepr (Numero x) n = int x
termRepr (Estructura s ts) n = 
    if null ts 
        then text s 
        else nestify n s ts

nestify :: Int -> String -> [Termino] -> Doc
nestify n s ts = text s <> lparen $$ foldl ($$) empty (punctuate comma $ map (\t -> nest (n+2) $ termRepr t n) ts) $$ rparen