{
module Parser where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  TkNum { TokenNum $$ }
  TkString { TokenString $$ }
  TkId { TokenId $$ }
  TkLit { TokenLit $$ }
%%

Grammar : {- empty -}                               { EmptyGrammar }
        | Rule Grammar                              { Rule $1 $2 }

Rule : TkId Productions                             { (snd $1, $2) }

Productions : {- empty production -}                { EmptyProduction }
            | Production Productions                { MkProduction $1 $2 }

Production : '|' Expansion '=>' Term                { ($1, $2) }

Expansion : {- empty -}                             { EmptyExpansion }
          | Symbol Expansion                        { Expansion $1 $2 }

Symbol : 'ID'                                       { SymID }
       | 'STRING'                                   { SymSTRING }
       | 'NUM'                                      { SymNUM }
       | TkString                                   { SString $1 }
       | TkId                                       { SId $1 }

Term : '_'                                          { Hole }
     | TkId Args                                    { TId $2 }
     | TkString                                     { TString $1 }
     | TkNum                                        { TNum $1 }
     | '$' TkNum Sustitution                        { TSust $1 $2 }

Args : {- empty -}                                  { [] }
     | '(' ArgList ')'                              { $2 }

ArgList : {- empty -}                               { [] }
        | Term ArgListCont                          { $1 : $2 }

ArgListCont : {- empty -}                           { [] }
            | ',' Term ArgListCont                  { $1 : $2 }

Sustitution : {- empty -}                            { EmptySustitution }
           | '[' Term ']'                           { Sustitution $2 }

{

parseError :: [Token] -> a
parseError (token:tokens) = error ("Parse error: invalid symbol \"" ++ token)

data Grammar = EmptyGrammar
             | Rule (String, [Productions]) Grammar

data Productions = EmptyProduction
                 | MkProduction (Expansion, Term) Productions

data Expansion = EmptyExpansion
               | Expansion Symbol Expansion
    
data Symbol = SymID
            | SymSTRING 
            | SymNUM
            | SString String 
            | SId String

data Term = Hole 
          | TId String [Term]
          | TString String 
          | TNum Int 
          | TSust Int Sustituion

data Sustitution = EmptySustitution
                 | Sustitution Term

}