{
module Parser where

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
        | Rule Grammar                              { Rule (fst $1) (snd $1) : $2 }

Rule : TkId Productions                             { ($1, $2) }

Productions : {- empty production -}                { [] }
            | Production Productions                { Production (fst $1) (snd $1) : $2 }

Production : '|' Expansion '=>' Term                { ($2, $4) }

Expansion : {- empty -}                             { [] }
          | Symbol Expansion                        { $1 : $2 }

Symbol : 'ID'                                       { SymID }
       | 'STRING'                                   { SymSTRING }
       | 'NUM'                                      { SymNUM }
       | TkString                                   { SString $1 }
       | TkId                                       { SId $1 }

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

type Grammar = [Rule]

data Rule = Rule String [Production] deriving (Eq, Show)

data Production = Production [Symbol] Term deriving (Eq, Show)
    
data Symbol = SymID
            | SymSTRING 
            | SymNUM
            | SString String 
            | SId String deriving (Eq, Show)

data Term = Hole 
          | TId String [Term]
          | TString String 
          | TNum Int 
          | TSust Int (Maybe Term) deriving (Eq, Show)

}