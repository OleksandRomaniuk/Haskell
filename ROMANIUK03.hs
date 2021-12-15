{-# OPTIONS_GHC -Wall #-}
module ROMANIUK03 where
import Data.List
import Data.Maybe (fromJust)
type Graph  = [[Int]]


nodes:: Graph -> [Int]
nodes g = [0..(length g - 1)]

edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x<-nodes g, y<- g!!x]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph [] = True
isGraph [[_]] = False
isGraph (v:gr) = (hasAllDifferent v) && (isGraph gr)
hasAllDifferent :: [Int] -> Bool
hasAllDifferent [] = True
hasAllDifferent [_] = True
hasAllDifferent (x:xs) = (not (x `elem` xs)) && (hasAllDifferent xs)

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool
isTournament gr = null [(x,y) | x <- [0..(length gr)-1], y <- [0..(length gr)-1],
                                (elem y (gr!!x)) `xor` (notElem x (gr!!y)), x /= y]

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

allEdges :: Graph -> [(Int,Int)]
allEdges gr = [(x,y)| x<-[0..(length gr - 2)], y <- [x+1..(length gr - 1)]]

containsAllSimilar :: [(Int,Int)] -> [(Int,Int)] -> Bool
containsAllSimilar _ [] = True
containsAllSimilar ys (s:xs) | containsSimilar ys s = containsAllSimilar ys xs
                             | otherwise = False 

containsSimilar :: [(Int,Int)] -> (Int,Int) -> Bool
containsSimilar [] _ = False
containsSimilar (s:xs) y | s==y || ((fst s)==(snd y) && (fst y)==(snd s)) = True
                         | otherwise = containsSimilar xs y

-- Задача 3 ------------------------------------

findAllTransitive :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
findAllTransitive _ [] = []
findAllTransitive x xs = [((fst x),j)|(i,j)<-xs, i==(snd x)]

isTransitive :: Graph -> Bool 
isTransitive g = and [existWay g (v,u1) (u2,w) | (v,u1) <- edges g, (u2,w) <- edges g, u1 == u2]

existWay :: Graph -> (Int,Int) -> (Int,Int) -> Bool
existWay g (v,_) (_,w) = (v,w) `elem` edges g

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive gr = edgesToGraph gr (snd (until condition4 step4 (init4(edges gr))))

condition4 :: ([(Int,Int)],[(Int,Int)]) -> Bool
condition4 (new, _) = null new

step4 :: ([(Int,Int)],[(Int,Int)]) -> ([(Int,Int)],[(Int,Int)])
step4 (ns, os) = (difference new old, old)
 where old = ns++os
       new = difference (set(foldl1 (++) [findAllTransitive x old|x<-old])) old 

init4 :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
init4 [] = ([], [])
init4 os = (difference (set(foldl1 (++) [findAllTransitive x os|x<-os])) os,os)

set :: [(Int,Int)] -> [(Int,Int)]
set [] = []
set (s:xs) = s:set(filter(/=s)xs)

difference :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
difference xs ys = set (xs\\ys)

edgesToGraph :: Graph -> [(Int,Int)] -> Graph 
edgesToGraph _ [] = []
edgesToGraph gr edgs = [sort [snd x|x<-edgs, (fst x) == z]| z <- [0..(length gr - 1)]]

-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr v u | isGraph gr = if (longWayAssist gr v u) == [] then Nothing
 else Just (longWayAssist gr v u)
               | otherwise = error "graph must be oriented"

longWayAssist :: Graph -> Int -> Int -> [Int] 
longWayAssist gr v u = reverse(theLongest(toListWithEnd u (allSimpleWays gr v))) 

allSimpleWays :: Graph -> Int -> [[[Int]]] 
allSimpleWays gr v = until cond5 (step5 gr) [[[v]]]

cond5 :: [[[Int]]] -> Bool 
cond5 wss = null(head wss)

step5 :: Graph -> [[[Int]]] -> [[[Int]]] 
step5 gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, t<-gr!!x, notElem t xs]:wss
step5 _ [] = error "allWays:stepW"

toListWithEnd :: Int -> [[[Int]]] -> [[Int]]
toListWithEnd u wws = [x | ws<-wws, x<-ws, head x == u]

theLongest :: [[Int]] -> [Int]
theLongest [] = []
theLongest (x:[]) = x
theLongest (x:[y]) | length x > length y = x
                   | otherwise = y
theLongest (x:xs) | length x > length (head xs) = theLongest (x:(tail xs))
                  | otherwise = theLongest xs

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | null cycles = Nothing
               | otherwise = Just (head cycles)
    where cycles = [res | x <- [0..(length gr)-1],
                          res <- (allCycles gr x x []),
                          length res - 1 == length gr]

allCycles :: Graph -> Int -> Int -> [Int] -> [[Int]]
allCycles gr st end used | st == end && not (null used) = [[end]]
                         | elem st used = []
                         | otherwise = [st:step | nxt <- (gr!!st),
                           step <- (allCycles gr nxt end (st:used))]


-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = foldl1 (&&) [snd(allWays gr x)|x <- [0..(length gr - 1)]]

allWays :: Graph -> Int -> ([[[Int]]], Bool)
allWays gr v = until cond7 (step7 gr) ([[[v]]], True)

cond7 :: ([[[Int]]], Bool) -> Bool 
cond7 (wss, b) = null(head wss) || not b

step7 :: Graph -> ([[[Int]]], Bool) -> ([[[Int]]], Bool)
step7 gr (wss@(wsn:_), b) = ([t:w | w@(x:xs) <- wsn, t<-gr!!x, notElem x xs]:wss, foldl (&&) b [isSet y| y<-wsn])
step7 _ ([], _) = error "allWays:stepW"

isSet :: [Int] -> Bool
isSet [] = True
isSet (s:xs) | elem s xs = False
             | otherwise = isSet xs

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr | isAcyclic gr = Just (snd (until cond1 step1 (0, [])))
             | otherwise = Nothing
    where cond1 :: (Int, [Int]) -> Bool -- For iterating on gr
          cond1 (n, _) = n >= length gr
          step1 :: (Int, [Int]) -> (Int, [Int])
          step1 (n, res1) | notElem n nxt1 = (n+1, n:nxt1)
                          | otherwise = (n+1, nxt1)
              where nxt1 = snd (until cond2 step2 (0, res1)) -- For iterating on gr!!n
                    cond2 :: (Int, [Int]) -> Bool
                    cond2 (m, _) = m >= length (gr!!n)
                    step2 :: (Int, [Int]) -> (Int, [Int])
                    step2 (m, res2) = (m+1, snd (step1 (gr!!n!!m, res2)))

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr ts
    | fromJust (topolSort gr) == ts = True
    | otherwise = False


gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]