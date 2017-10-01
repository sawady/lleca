module Generator where

import Lexer
import Parser
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad

type FirstSet = Map.Map Symbol (Set.Set Symbol)
type FollowSet = Map.Map Symbol (Set.Set Symbol)
type LLTable   = Map.Map (Symbol, Symbol) (Set.Set Production)

data Termino = 
    Agujero
  | Cadena String
  | Numero Int
  | Estructura String [Termino] deriving (Eq, Show)

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

generateTable :: [Symbol] -> [Symbol] -> Grammar -> FirstSet -> FollowSet -> LLTable
generateTable terminals metas grammar frsts fws = checkLL1 step
    where wmap = initialMap [ (x,y) | x <- metas, y <- DollarSign : terminals ]
          step = foldr (\p@(Production n syms _) rm -> addIfAnullable p (SymMeta n) syms $ addFromTerminals p rm (SymMeta n) syms) wmap grammar
          addFromTerminals p m a syms = Set.foldr (\x rm -> insertEntry p a x rm) m 
                                                  (Set.filter isTerminal $ takeFirstWithoutEpsilon syms)
          takeFirstWithoutEpsilon syms = Set.delete Epsilon $ firstOfString syms frsts
          insertEntry p a x m = Map.update (\set -> Just $ Set.insert p set) (a, x) m
          addIfAnullable p a syms m = 
            if Set.member Epsilon $ firstOfString syms frsts
                then Set.foldr (\x rm -> insertEntry p a x rm) m (lookupM a fws)
                else m

checkLL1 s = if all (\s -> Set.size s <= 1) (Map.elems s) then s else error "The grammar is not LL(1)"

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

parseTermino :: String -> String -> IO Termino
parseTermino grammar input = 
    let tokenizedGrammar = lexer llecaKeywords llecaSymbols grammar
        parsedGrammar = parse tokenizedGrammar
        (kws, syms) = calcKeywords parsedGrammar
        metas = metaSymbols parsedGrammar
        terminals = terminalSymbols parsedGrammar
        frset = firstSet terminals metas parsedGrammar
        foset = followSet metas parsedGrammar frset
        table = generateTable terminals metas parsedGrammar frset foset
        tokenizedInput = lexer kws syms input
        firstSymbol = head metas
        in do mapM_ print (Map.assocs table)
              mapM_ print (tokenizedInput ++ [TokenDollar])
              return $ analize firstSymbol table (tokenizedInput ++ [TokenDollar])

analize :: Symbol -> LLTable -> [Token] -> Termino
analize x table ts = 
    case evalState (runExceptT $ analizeWithState x table) ts of
        Left e  -> error e
        Right x -> x

analizeWithState :: Symbol -> LLTable -> ExceptT String (State [Token]) Termino
analizeWithState x table = 
    if isTerminal x
        then do 
                b <- fmap head get
                modify tail
                if equalSym b x
                    then return $ leaf b
                    else throwError $ "Syntax error, expected: " ++ show x ++ " but " ++ show b ++ " given"
        else do
            b <- fmap head get
            let set = lookupM (x, toSym b) table
            when (Set.null set) (throwError $ "Syntax error, expected: " ++ show x ++ " but " ++ show b ++ " given")
            let (Production n syms act) = head (Set.elems set)
            args <- forM syms (`analizeWithState` table)
            return (generateAction act args)

generateAction :: TermAction -> [Termino] -> Termino
generateAction Hole _ = Agujero
generateAction (TString s) _ = Cadena s
generateAction (TNum n) _ = Numero n
generateAction (TId s as) args = Estructura s (map (`generateAction` args) as) 
generateAction (TSust i Nothing) args = args !! (i-1)
generateAction (TSust i (Just sust)) args = fillHole (args !! (i-1)) (generateAction sust args)

fillHole :: Termino -> Termino -> Termino
fillHole Agujero t = t
fillHole (Estructura s ts) t = Estructura s (map (`fillHole` t) ts)
fillHole x _ = x

