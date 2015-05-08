module CYK where

import CFGrammar

type P = [[[Bool]]]

parse :: String -> CFG -> Bool
parse w g = get 1 n startIndex . secondLoop w g . firstLoop w g $ initP w g
        where n = length w
              startIndex = ntIndex g $ start g

--initial array
initP :: String -> CFG -> [[[Bool]]] 
initP w g = replicate n . replicate n $ replicate (length $ nonTerm g) False
        where n = length w

firstLoop :: String -> CFG -> P -> P
firstLoop w g p = foldl'' p [1..n] (\cur_i i ->
                        foldl'' cur_i unitProds (\cur_j (Rule b (c:xs)) ->
                                if Term (w !! (i-1)) == c then
                                        markArrPos i 1 (ntIndex g b) cur_j else cur_j))
                  where n = length w
                        unitProds = unitProd $ rules g

secondLoop :: String -> CFG -> P -> P
secondLoop w g p = foldl'' p [2..n] (\cur_i i -> 
                        foldl'' cur_i [1..n-i+1] (\cur_j j -> 
                                foldl'' cur_j [1..i-1] (\cur_k k ->
                                        foldl'' cur_k (nonUnitProd g) $ setP i j k g)))
                   where n = length w  
                   
--if P[j,k,B] and P[j+k,i-k,C] then set P[j,i,A] = true                             
setP :: Int -> Int -> Int -> CFG -> P -> Rule -> P
setP i j k g p (Rule h (x:xs)) = if get j k b p && get (j + k) (i - k) c p then
                                        markArrPos j i a p else p
        where   a = ntIndex g h
                b = ntIndex g x
                c = ntIndex g $ head xs
                
--mark array position as true at p[i,j,k]
markArrPos :: Int -> Int -> Int -> P -> P
markArrPos i j k p = replace i p . replace j (p !! (i-1)) $ replace k (p !! (i-1) !! (j-1)) True

get :: Int -> Int -> Int -> P -> Bool
get i j k p = p !! (i-1) !! (j-1) !! (k-1)

foldl'' :: a -> [b] -> (a -> b -> a) -> a
foldl'' initial list f = foldl f initial list

replace :: Int -> [a] -> a -> [a]
replace _ [] _ = []
replace 1 (x:xs) elm = elm : xs
replace i (x:xs) elm = x : replace (i - 1) xs elm