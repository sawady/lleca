{
module Parser where

import Data.Char
import Data.List
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  'ID' { TokenLit "ID"  }
  'NUM' { TokenLit "NUM"  }
  'STRING' { TokenLit "STRING" }
  '_' { TokenLit "_" }
  '|' { TokenLit "|" }
  '=>' { TokenLit "=>" }
  '$' { TokenLit "$" }
  '(' { TokenLit "(" }
  ')' { TokenLit ")" }
  ',' { TokenLit "," }
  '[' { TokenLit "[" }
  ']' { TokenLit "]" }
  TkNum { TokenNum $$ }
  TkString { TokenString $$ }
  TkId { TokenId $$ }
%%

Grammar : {- empty -}                               { [] }
        | Rule Grammar                              { $1 ++ $2 }

Rule : TkId Productions                             { map (\(e, t) -> Production $1 e t) $2 }

Productions : {- empty production -}                { [] }
            | Production Productions                { $1 : $2 }

Production : '|' Expansion '=>' Term                { ($2, $4) }

Expansion : {- empty -}                             { [] }
          | Symbol Expansion                        { $1 : $2 }

Symbol : 'ID'                                       { SymID }
       | 'STRING'                                   { SymSTRING }
       | 'NUM'                                      { SymNUM }
       | TkString                                   { SymLit $1 }
       | TkId                                       { SymMeta $1 }

Term : '_'                                          { Hole }
     | TkId Args                                    { TId $1 $2 }
     | TkString                                     { TString $1 }
     | TkNum                                        { TNum $1 }
     | '$' TkNum Sustitution                        { TSust $2 $3 }

Args : {- empty -}                                  { [] }
     | '(' ArgList ')'                              { $2 }

ArgList : {- empty -}                               { [] }
        | Term ArgListCont                          { $1 : $2 }

ArgListCont : {- empty -}                           { [] }
            | ',' Term ArgListCont                  { $2 : $3 }

Sustitution : {- empty -}                           { Nothing }
           | '[' Term ']'                           { Just $2 }

{

parseError :: [Token] -> a
parseError (token:tokens) = error ("Parse error: invalid symbol \"" ++ show token)

type Grammar = [Production]

data Production = Production String [Symbol] TermAction deriving (Ord, Eq, Show)

data Symbol = SymID
            | SymSTRING 
            | SymNUM
            | SymLit String 
            | SymMeta String
            | Epsilon 
            | DollarSign deriving (Read, Eq, Ord, Show)

data TermAction = Hole 
                | TId String [TermAction]
                | TString String 
                | TNum Int 
                | TSust Int (Maybe TermAction) deriving (Ord, Eq, Show)

llecaKeywords = ["_", "ID", "STRING", "NUM"]
llecaSymbols  = ["|", "=>", "$", "(", ")", ",", "[", "]"]

calcKeywords :: Grammar -> ([String], [String])
calcKeywords g = partition isVar (concat $ map collect g)
    where collect (Production _ syms _) = map get $ filter isLit syms
          get (SymLit s) = s
          isVar xs = head xs == '_' || isLetter (head xs)

isLit :: Symbol -> Bool
isLit (SymLit s) = True
isLit _          = False

isTerminal :: Symbol -> Bool
isTerminal (SymMeta _) = False
isTerminal _           = True

initialMetaSymbol :: Grammar -> Symbol
initialMetaSymbol = head . metaSymbols

metaSymbols :: Grammar -> [Symbol]
metaSymbols g = nub $ map (\(Production n _ _) -> SymMeta n) g

terminalSymbols :: Grammar -> [Symbol]
terminalSymbols g = nub $ concat $ map (\(Production _ syms _) -> filter isTerminal syms) g

}