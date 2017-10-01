module Generator where

import Lexer
import Parser
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

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

firstSet :: [Symbol] -> [Symbol] -> Grammar -> Map.Map Symbol (Set.Set Symbol)
firstSet terminals metas grammar = withMeta
    where initialMap      = foldr (\s m -> Map.insert s Set.empty m) Map.empty metas
          withTerminals   = foldr (\s m -> Map.insert s (Set.singleton s) m) initialMap terminals
          withMeta        = untilNoChanges $ iterate (\m -> foldr (\(Production n syms _) m -> addFirstToMeta (SymMeta n) syms m) m grammar) withTerminals
          addFirstToMeta ms syms m = 
            if null syms
                then addEpsilon ms m
                else if isTerminal (head syms)
                        then Map.update (\set -> Just $ Set.insert (head syms) set) ms m
                        else addEpsilonIfAllNullable ms syms (foldr (\i mr -> addIfAnullable ms (take i syms) mr) m [1 .. length syms])
          addIfAnullable ms syms m = 
            if null (init syms) || nullable (init syms) m
               then Map.update (\set -> Just $ Set.union set (Set.delete Epsilon $ lookupM (last syms) m)) ms m
               else m
          nullable syms m = Set.member Epsilon $ foldr1 (\s r -> Set.intersection s r) $ map (\s -> lookupM s m) syms
          addEpsilonIfAllNullable ms syms m = 
            if nullable syms m
                then addEpsilon ms m
                else m
          lookupM k m = fromJust $ Map.lookup k m
          addEpsilon ms m = Map.update (\set -> Just $ Set.insert Epsilon set) ms m

untilNoChanges :: Eq a => [a] -> a
untilNoChanges (x:y:xs) = 
    if x == y
       then x
       else untilNoChanges (y:xs)