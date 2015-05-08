module CFGrammar where

import Data.List

data Symbol = Term Char | Nonterm Char deriving (Eq, Show)
data Rule = Rule Symbol [Symbol]
data CFG = CFG [Symbol] [Symbol] Symbol [Rule]

instance Show CFG where
        show (CFG n sigma s rules) = "Nonterminals:\n" ++ show n ++
                "\nTerminals:\n" ++ show sigma ++
                "\nStart symbol:\n" ++ show s ++
                "\nProduction rules:\n" ++ printRules rules 
                
instance Show Rule where
        show (Rule lhs rhs) = show lhs ++ " -> " ++ foldl (++) "" (map show rhs)

printRules :: [Rule] -> String
printRules [] = ""
printRules (x:xs) = show x ++ "\n" ++ printRules xs

elemSymbol :: Char -> [Symbol] -> Bool
elemSymbol a symbols = Term a `elem` symbols || Nonterm a `elem` symbols 

nonTerm :: CFG -> [(Symbol, Int)]
nonTerm (CFG n _ _ _) = zip n [1..length n]

start :: CFG -> Symbol
start (CFG _ _ s _) = s

rules :: CFG -> [Rule]
rules (CFG _ _ _ r) = r

unitProd :: [Rule] -> [Rule]
unitProd [] = []
unitProd (r@(Rule a b):xs)
        | length b == 1 = r : unitProd xs
        | otherwise = unitProd xs
        
nonUnitProd :: CFG -> [Rule]
nonUnitProd (CFG _ _ _ r) = filter (\(Rule a b) -> length b == 2) r
        
--Get the index of the nonterminal symbol (for the CYK table)
ntIndex :: CFG -> Symbol -> Int
ntIndex g a = case find (\(x, y) -> x == a) (nonTerm g) of
        Just (x,y) -> y
        Nothing -> error "Nonterminal not found"

buildGrammar :: [String] -> CFG
buildGrammar (n:s:sig:xs) = let nonterm = map Nonterm $ parseSym n
                                sigma = map Term $ parseSym sig 
                                st = Nonterm $ head s
                                in CFG nonterm sigma st (parseRules nonterm sigma xs)

parseSym :: String -> [Char]
parseSym = filter (' ' /=)

--Given a list of strings, parse each string into a CNF rule
parseRules :: [Symbol] -> [Symbol] -> [String] -> [Rule]
parseRules nonterm sig (x:xs) = map (parseRule nonterm sig) xs

--Given a string, parse into a CNF rule
parseRule :: [Symbol] -> [Symbol] -> String -> Rule
parseRule nonterm sig str = let (x:xs) = words str in
        Rule (Nonterm (head x)) . map (cnfRHSParse nonterm sig) $ head xs

--parse the RHS of the rule given nonterminals and terminals
cnfRHSParse :: [Symbol] -> [Symbol] -> Char -> Symbol
cnfRHSParse nonterm sig x
        | x `elemSymbol` nonterm = Nonterm x
        | x `elemSymbol` sig = Term x
        | otherwise = error "symbol in production rule not in grammar"