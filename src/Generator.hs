module Generator where

import Lexer
import Parser
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

type FirstSet = Map.Map Symbol (Set.Set Symbol)
type FollowSet = Map.Map Symbol (Set.Set Symbol)

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

firstSet :: [Symbol] -> [Symbol] -> Grammar -> FirstSet
firstSet terminals metas grammar = withMeta
    where withTerminals   = foldr (\s m -> Map.insert s (Set.singleton s) m) (initialMap metas) terminals
          withMeta        = untilNoChanges $ iterate (\m -> foldr (\(Production n syms _) m -> addFirstToMeta (SymMeta n) syms m) m grammar) withTerminals
          addFirstToMeta ms syms m = 
            if null syms
                then addEpsilon ms m
                else addEpsilonIfAllNullable ms syms (foldr (\i mr -> addIfAnullable ms (take i syms) mr) m [1 .. length syms])
          addIfAnullable ms syms m = 
            if null (init syms) || nullable (init syms) m
               then Map.update (\set -> Just $ Set.union set (Set.delete Epsilon $ lookupM (last syms) m)) ms m
               else m
          addEpsilonIfAllNullable ms syms m = 
            if nullable syms m
                then addEpsilon ms m
                else m
          addEpsilon ms m = Map.update (\set -> Just $ Set.insert Epsilon set) ms m

initialMap syms = foldr (\s m -> Map.insert s Set.empty m) Map.empty syms

lookupWihoutEpsilon syms m = Set.delete Epsilon $ lookupM (last syms) m

lookupM k m = fromJust $ Map.lookup k m

nullable syms m = Set.member Epsilon $ foldr1 (\s r -> Set.intersection s r) $ map (\s -> lookupM s m) syms

untilNoChanges :: Eq a => [a] -> a
untilNoChanges (x:y:xs) = 
    if x == y
       then x
       else untilNoChanges (y:xs)

firstOfString :: [Symbol] -> FirstSet -> Set.Set Symbol
firstOfString syms fs = if null syms then Set.singleton Epsilon else addEpsilonIfAllNullable addFirsts
    where addFirsts = foldr (\i s -> addIfAnullable (take i syms) s) Set.empty [1 .. length syms]
          addIfAnullable syms s =
            if null (init syms) || nullable (init syms) fs
               then Set.union s (lookupWihoutEpsilon syms fs)
               else s
          addEpsilonIfAllNullable s = 
            if nullable syms fs
                then Set.insert Epsilon s
                else s

followSet :: [Symbol] -> Grammar -> FirstSet -> FollowSet
followSet metas grammar fs = untilNoChanges $ iterate step2 step1
    where wmap = Map.insert (head metas) (Set.singleton DollarSign) (initialMap metas)
          step1 = foldr (\(Production n syms _) m -> addMetaFirsts (calcBetas syms) m) wmap grammar
          step2 = \m -> foldr (\(Production n syms _) m -> addMetaA (SymMeta n) (anullableBetas $ calcBetas syms) m) m grammar
          addMetaFirsts symsB m = 
            foldr (\(s, ss) rm -> Map.update (\set -> Just $ Set.union set $ Set.delete Epsilon $ firstOfString ss fs) s rm) m symsB 
          calcBetas [] = []
          calcBetas (s:ss) = 
            if not $ isTerminal s
                then (s, ss) : calcBetas ss
                else calcBetas ss
          anullableBetas bs = map fst $ filter (\(s, ss) -> Set.member Epsilon (firstOfString ss fs)) bs 
          addMetaA s syms m = foldr (\s' rm -> Map.update (\set -> Just $ Set.union (lookupM s' rm) (lookupM s rm)) s' rm) m syms
