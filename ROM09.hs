{-# OPTIONS_GHC -Wall #-}
module ROMANIUK09 where

import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Data.List
import Data.Maybe(fromJust, isJust)

data Term   =  Nmb Int         -- десяткове число без знаку
            | Var String       -- змінна, довільний ідентифікатор
            | App Term Term    -- операція застосування
            | Abs String Term  --  операція абстракції
           deriving (Show, Eq) 
type Contex = [(String,Term)]

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String] 
addVar q [] = [q]
addVar q list = nub(q:list)
-- Задача 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar _ [] = []
delVar q list = if (q == head list) then tail list
                  else (head list):delVar q (tail list)
-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV q w = nub(q ++ w)
-- Задача 1.d ----------------------------------------- 
freeVars    :: Term -> [String]
freeVars t = f2 t []  

f2    :: Term -> [String] -> [String]
f2 (Nmb _) v = v
f2 (Var vr) v = addVar vr v
f2 (App t11 t22) v = unionV (f2 t11 v) (f2 t22 v)
f2 (Abs m tr) v = delVar m (f2 tr v)
-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn _ [] = []
deleteSyn x c = if (x==(fst(head c))) then deleteSyn x (tail c)
                    else (head c):(deleteSyn x (tail c))

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm termmm fun = filter (\a -> a `elem` map fst fun) (freeVars termmm) == (freeVars termmm)



-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex iscontext = iswfc iscontext 1  where
                         iswfc a b
                                | b > (length a) = True
                                | iswfTerm (snd $(a!!(b-1))) a = iswfc a $(b+1)
                                | otherwise = (False)
-- Задача 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber (Nmb _) = True 
isNumber (Var _) = False
isNumber (App _ _ ) = False
isNumber (Abs str tr)= b1 tr str

b1 :: Term -> String  -> Bool
b1 (Nmb _) _ = False 
b1 (Var _) _ = False
b1 (App _ _ ) _ = False
b1 (Abs str1 tr) str = b22 tr str str1 


-- Задача 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber tr = if (isNumber tr) then ter tr 0
              else tr 
ter :: Term -> Int -> Term
ter (Var _) coun = (Nmb coun)
ter (App _ tr) coun = ter tr (coun+1)
ter (Abs _ (Abs _ tr )) coun = ter tr coun
ter tr _ = tr

-- Задача 3.c -----------------------------------------
compress ::  Term -> Term
compress tr = if (isNumber tr)==True then (inNumber tr)
              else case tr of 
              (Abs s tr1) -> (Abs s (compress tr1))
              (App tr1 tr2) -> (App (compress tr1) (compress tr2))
              (Var r) -> Var r
              (Nmb r) -> Nmb r

-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term 
reduce q e s  = case q of
                Nmb a -> Nmb a
                Var a -> if(a==e) then s else Var a
                (Abs a term) -> let z = (newVar (unionV (freeVars s) (freeVars term)) a)
                                in if(a==e) then Abs a term 
                                   else if(not (elem a (freeVars s))) then (Abs a (reduce term e s)) 
                                        else if (not (elem z (unionV (freeVars s) (freeVars term)))) 
                                             then Abs z (reduce (reduce term a (Var z)) e s) 
                                             else error "Thic ic unknown reduction"
                (App t11 t22) -> App (reduce t11 e s) (reduce t22 e s)
-- Задача 5 -----------------------------------------
reFunc :: String -> Contex -> Maybe Term
reFunc _ [] = Nothing
reFunc q (c:cnt) | ((fst c) == q) = Just (snd c)
                  | otherwise = reFunc q cnt

evalStep :: Term -> Contex -> Maybe Term   
evalStep w cnt | (w == newt) = Nothing
               | otherwise = Just newt
                 where newt = rewrite w cnt (freeVars w)

-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term 
eval = undefined 

-- Задача 7 -----------------------------------------
parseTerm :: String -> Maybe Term 
parseTerm = undefined 


rewrite :: Term -> Contex -> [String] -> Term
rewrite (Nmb n) _ _ = (integerTerm n)
rewrite (Var v) cnt vars | (isJust tr) && (elem v vars) = fromJust(tr)
                                 | otherwise = (Var v)
                                   where tr = (reFunc v cnt)
rewrite (App t1 t2) cnt vars = case t1 of
                                        (Abs s t0) -> reduce t0 s t2
                                        _ -> if(nt == t1) then App nt (rewrite t2 cnt vars) else App nt t2
                                        where nt = (rewrite t1 cnt vars)
rewrite (Abs e y) cnt vars = Abs e (rewrite y (deleteSyn e cnt) vars)


b22 :: Term -> String -> String -> Bool
b22 (Nmb _) _ _ = False 
b22 (Var z) _ str1 = if(z == str1) then True  else False 
b22 (App tr1 tr2) str1 str2 = b33 tr1 tr2 str1 str2  
b22 (Abs _ _) _ _ = False
f1    :: Term -> [String] -> [String]
f1 (Nmb _) e = e
f1 (Var vr) e = addVar vr e
f1 (App t11 t22) e = unionV (f1 t11 e) (f1 t22 e)
f1 (Abs m tr) e = delVar m (f1 tr e)
b33 :: Term -> Term -> String -> String -> Bool
b33 (Var s) (Var z) str1 str2 = if (s ==str1) && (z==str2) then True
                                       else False
b33 (Var s) (App tr1 tr2) str1 str2 = if s==str1 then b33 tr1 tr2 str1 str2  
                                              else False
b33 _ _ _ _ = False

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 
        buildTerm j = (App (Var "s") (buildTerm (j-1)))  

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n 
        next (c:cx) | isDigit c = (succ c):cx 
        next n      = '0':n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z")))) 
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            ) 
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String 
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"