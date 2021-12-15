{-# OPTIONS_GHC -Wall #-}
module ROMANIUK07 where 
import Data.List (sortBy,find)
import Data.Function (on)

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne] 
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne
coef 0 _ = []
coef n p = [ (fst m, snd m*n) | m <-p]

-- Задача 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add p1 p2 = mySort (clean (coef 1 (addHelper p1 p2 ++ [ n |n<-p1, fst n `notElem` indPol (addHelper p1 p2) ] ++ [ m | m<-p2, fst m `notElem` indPol (addHelper p1 p2) ])))
mySort :: Ord a => [(a, b)] -> [(a, b)]
mySort = sortBy ( compare `on` fst)
addHelper :: PolinomOne -> PolinomOne -> PolinomOne
addHelper p1 p2 = [ (fst m, snd m + sndn (fst m) p2) | m <- p1, fst m `elem` fstn]
 where fstn = [fst n |n <- p2]
sndn :: Int -> PolinomOne -> Rational
sndn i pol = sum [snd n | n <- pol, i == fst n]
indPol :: PolinomOne -> [Int]
indPol p = [fst n | n<-p]
clean :: PolinomOne -> PolinomOne
clean p = [ n | n<-p, snd n /=0]

-- Задача 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne
unify p = mySort [ elem1 n p | n<-listfun p]

elem1 :: Int -> PolinomOne -> (Int,Rational)
elem1 i p1 = (i, sum [snd n | n <- p1, fst n == i])
listfun :: PolinomOne -> [Int]
listfun p1 = chnumb[fst n | n <-p1]
chnumb :: (Eq b) => [b] -> [b]
chnumb (y:ys) = y : chnumb (filter (/= y) ys)
chnumb [] = []

-- Задача 2.a -----------------------------------------
check :: PolinomOne -> Bool
check [(_, p1)] = p1 == 1
check _ = False
findPl :: [PolinomOne ]-> Int -> [Int]
findPl [] _ = []
findPl [x] ind
    | check x = [ind]
    | otherwise = []
findPl (p11:p22) ind
    | check p11 = ind : findPl p22 (ind + 1)
    | otherwise = findPl p22 (ind + 1)
findFree :: [PolinomOne] -> [Int]
findFree p = findPl p 1

-- Задача 2.b -----------------------------------------
chc :: [Int] -> PolinomOne -> Bool
chc _ [] = False
chc [] ((i1, _):p2) = i1 /= 0 || chc [] p2
chc fre [(i1, _)] = i1 /= 0 && notElem i1 fre
chc fre ((i1, _):p2) = (i1 /= 0 && notElem i1 fre) || chc fre p2
iswfCommon ::  [PolinomOne]  -> Bool
iswfCommon [] = False
iswfCommon [p] = check p
iswfCommon p = let fr = findFree p
                   in not (any (chc fr) p)

-- Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple lin = not (any (\x -> length x /= 1) lin)

-- Задача 3.b -----------------------------------------
notch :: Row -> Bool
notch r = length r /= 1 || head r /= 0
solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple lin
    | not (isSimple lin) = Nothing
    | otherwise = if any notch lin then Nothing else Just []

-- Задача 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow lin = fun1 lin 1
    where
        fun1 :: Linear -> Int -> Maybe Int
        fun1 [] _ = Nothing
        fun1 (l:ls) z   | null l = fun1 ls (z+1)
                            | head l /= 0 = Just z
                            | otherwise = fun1 ls (z+1)
-- Задача 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow list i = let 
                uns = i > 1 && i <= (length list)
                elem0 = list !! 0
                elemI = list !! (i - 1)
                ser = take (i-2) (tail list)
                last = drop (i) list
                in if uns then elemI : (ser ++ (elem0 : last)) else list

-- Задача 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep [] _ = []
forwardStep _ [] = []
forwardStep s1 (l:ls) = let 
                                firstRow = l
                                d1 = coe (-( (head firstRow) / (head s1) )) s1
                                q1 = addmun d1 firstRow
                            in tail q1 : forwardStep s1 ls 

coe :: Rational -> Row -> Row
coe c r = fmap (*c) r

addmun :: Row -> Row -> Row
addmun a b = zipWith (+) a b

-- Задача 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep r answ = sumpolin (rowForm r answ) : answ

rowForm :: Row -> [PolinomOne] -> [PolinomOne]
rowForm row pols = elp row pols 0 (head row)
    where 
        elp :: Row -> [PolinomOne] -> Int -> Rational -> [PolinomOne]
        elp _ [] _ _ = []
        elp [] _ _ _ = []
        elp (y:z) (p:ps) c ai1    | c == 0 = [(0,((head (reverse row)) / ai1))] : elp z  (p:ps) (c+1) ai1
                                        | otherwise = coef (-(y / ai1)) p : elp z ps (c+1) ai1

sumpolin :: [PolinomOne] -> PolinomOne
sumpolin [] = []
sumpolin (p:ps) = add p (sumpolin ps)

-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne] 
gauss = undefined

-- Задача 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool 
testEquation = undefined

-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool 
testLinear = undefined

-- Задача 8 -----------------------------------------
solving :: Linear -> Solution  
solving = undefined

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne 
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty 
sol2 = Empty 
sol3 = Many res3 
sol4 = One [62/15, -17/15, -4/3] 


