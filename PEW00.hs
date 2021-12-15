{-# OPTIONS_GHC -Wall #-}
module ROMANIUK00 where

type Graph  = [[Int]]
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                  deriving (Show, Eq)
type System = [(String,Recur)] 
data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)

-- Задача 1 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group [] = []
group (y:ys) = a : group b
    where
        (a, b) = span (== y) (y:ys)
   
-- Задача 2 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (x:y) res | elem x res = bagSubbag y (f1 res x)| otherwise = False

f1:: String -> Char -> String
f1 [] _ = ""
f1 (x:y) res | x==res = y| otherwise = [x] ++ f1 y res
-- Задача 3 -----------------------------------------
bagUnion :: String -> String -> String
bagUnion (x:xs) ys = let am1 = (resfunc xs x)+1
                         am2 = resfunc ys x
                         nxs = del xs x
                         nys = del ys x
                         res = if am1>am2 then (sot am1 x)
                               else (sot am2 x)
                     in res ++ (bagUnion nxs nys)
bagUnion [] ys = ys

resfunc :: String -> Char -> Int
resfunc (x:xs) y | y==x = 1 + (resfunc xs y)
                | otherwise = resfunc xs y
resfunc [] _ = 0
del :: String -> Char -> String
del (x:xs) y | x==y = del xs y
                | otherwise = [x]++(del xs y)
del [] _ = []
sot :: Int -> Char -> String
sot n ch = [ch | _<-[1..n]]

-- Задача 4 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency a = [(ad x a,x) | x <- f2 a]
f2 :: [Int] -> [Int]
f2 [] = []
f2 (x:xs) = x : filter (/=x) (f2 xs)

-- Задача 5 -----------------------------------------
components :: Graph -> [[Int]] 
components g = chelp g 0 []

chelp :: Graph -> Int -> [[Int]] -> [[Int]]
chelp g t r | t == length g = r| ret = chelp g (t+1) r| otherwise = chelp g (t+1) $ r ++ [hel] where ret = True `elem` [t `elem` x | x <- r]
                                                                                                     hel = [x | x <- [t..length g - 1], hw g t x]
-- Задача  6 -----------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay [] _ _ = []
shortWay g r t = let w = gn g t [[r]] in if w==[] then [] else w!!0

eccentricity :: Graph -> Int -> Int 
eccentricity g e = maximum [length (shortWay g e res) | res <- nhelp g] - 1

-- Задача 7 -----------------------------------------
findDiameter :: Graph -> Int
findDiameter gr = maximum $ eccent gr [0..length gr -1]

findRadius :: Graph -> Int
findRadius gr = minimum $ eccent gr [0..length gr -1]

eccent :: Graph -> [Int] -> [Int]
eccent gr (x:xs) = eccentricity gr x:eccent gr xs
eccent _ [] = []

-- Задача 8 -----------------------------------------
findCenter :: Graph -> [Int]
findCenter gr = let 
    findC:: Graph -> [Int] ->[Int]
    findC gr (x:xs) | radius == eccentricity gr x = x:findC gr xs
                    | otherwise = findC gr xs
                    where radius = findRadius gr
    findC _ [] = []
 in findC gr [0..length gr -1]

--- Задача 9 ----------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch a b = case a of
    NodeM a _ c d ->  if a == b then True else  if (a < b) then elemSearch d b  else elemSearch c b

-- Задача 10 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM q = NodeM q 1 EmptyM EmptyM
insSearch (NodeM z w l e) x = if x <  z then NodeM z w (insSearch l x) e else if x > z
            then NodeM z w l (insSearch e x) else NodeM z (w+1) l e

-- Задача 11 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _=  EmptyM
delSearch (NodeM a b c d) v | v == a = if b>1 
  then NodeM a (b-1) c d 
  else delhelp (NodeM a b c d )| v < a = NodeM a b (delSearch c v) d| otherwise = NodeM a b c (delSearch d v)

-- Задача 12 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList (y:ys) = (sortList (filter (<=y) ys)) ++ [y] ++ (sortList (filter (>y) ys))

-- Задача 13 ------------------------------------
isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst _ (Sel _ _) = True
isNumbConst syst (Super Succ f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst(Super Zero f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst (Name name) = isNumbConst syst (finder syst name)
isNumbConst _ _ = False

finder :: System -> String -> Recur
finder syst name = (snd . head) result where result = filter (( == name). fst) syst


containsRecur:: System -> String -> Bool
containsRecur syst s | null syst = False
                     | fst(head syst)==s = True
                     | otherwise = containsRecur (tail syst) s

cheknam :: Recur ->[String]
cheknam Zero = []
cheknam Succ = []
cheknam (Sel _ _ ) =[]
cheknam (Prim a b) = cheknam a ++ cheknam b
cheknam (Mini a _) = cheknam a
cheknam (Name a) = [a]
cheknam (Super a recurs) = (cheknam a) ++ concat [cheknam r2 | r2<-recurs]

stepEval::System -> Recur -> ([Int],Int) -> ([Int],Int)
stepEval syst r1 (xs,i)= let without1 = init xs
                             counter = (last without1)
                             without2 = init without1
                         in (without2 ++ [counter+1]++[eval syst r1 (without2++[counter]++[last xs])],i)

-- Задача 14 ------------------------------------
evRank :: System -> Recur -> Int 
evRank _ (Zero) = 1
evRank _ (Succ) = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = evRank syst (st)- 1 
evRank syst (Mini b _) = evRank syst b-1
evRank syst (Name f) = case (findRecur syst f) of 
    Just recu -> evRank syst recu
    Nothing -> 0

findRecur :: System -> String -> Maybe Recur
findRecur (x:xs) str | fst x == str = Just (snd x)
 | otherwise = findRecur xs str 
findRecur [] _ = Nothing

-- Задача 15 ------------------------------------
isNames :: System -> Bool 
isNames syst = rep syst && sub syst 

fff :: System -> Bool
fff syst = and [ (containsRecur (init syst)  r) | r<-(cheknam recur)]
                   where recur = (snd (last syst))
sub :: System->Bool
sub syst | null syst = True
                   | (length syst)==1 = and [ not (containsRecur syst r) | r<-cheknam (snd(head syst))]
                   | fff syst = sub (tail syst)
                   | otherwise = False
rep :: System->Bool
rep syst | null syst = True
               | containsRecur (tail syst) (fst(head syst)) = False
               | otherwise = rep (tail syst)
   
-- Задача 16 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel _ _)  = True
isRecur sys (Prim a b)  = isRecur sys a && isRecur sys b
isRecur sys (Mini a _)  = isRecur sys a
isRecur sys (Super a b) = isRecur sys a && and [isRecur sys x | x <- b]
isRecur sys (Name str) = checkRecurName (reverse sys) str

checkRecurName::System->String->Bool
checkRecurName [] _ = False
checkRecurName (x:sys) f = (fst x == f) || checkRecurName sys f

-- Задача 17 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval _ (Zero) _ = 0 
eval _ (Succ) xs= head xs + 1
eval _ (Sel _ x) xs = xs !! (x-1)
eval syst (Super f al) xs = eval syst f (map (\recu-> eval syst recu xs) al) 
eval syst (Name str) xs = case findRecur syst str of 
  Just recu -> eval syst recu xs  
  Nothing -> 0
eval syst (Prim r1 r2) xs =last $ fst $ until cond1 (stepEval syst r2) (init xs++[0]++ [eval syst r1 (take (evRank syst r1) xs)],last xs)
eval syst minRec@(Mini _ _) xs= case evalPart syst minRec xs of 
    Just res -> res
    Nothing -> 0
cond1::([Int],Int)->Bool
cond1 (xs,i)= i <= (xs !! (length xs -2))
-- Задача 18 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart _ (Zero) _ = Just 0 
evalPart _ (Succ) xs= Just (head xs + 1)
evalPart _ (Sel _ x) xs = Just (xs !! (x-1))
evalPart syst (Super f al) xs = case pre syst al xs [] of
    Just res -> evalPart syst f res
    Nothing -> Nothing                           
evalPart syst (Name str) xs = case findRecur syst str of 
  Just recu -> evalPart syst recu xs  
  Nothing -> Nothing
evalPart syst (Prim r1 r2) xs = case evalPart syst r1 (take (evRank syst r1) xs) of 
    Just initial -> case fst (until tyr (wrytefy syst r2) (Just (init xs++[0]++[initial]),last xs)) of
        Just zs -> Just (last zs)
        Nothing -> Nothing
    Nothing -> Nothing

evalPart syst (Mini b i) xs = let (ys,_,res)= until gh1 (dostep syst b) (Just (xs++[-1]),i,1)
 in case ys of 
    Just zs -> if (res==0) then Just (last zs) else Nothing 
    Nothing -> Nothing 


gn :: Graph -> Int -> [[Int]] -> [[Int]]
gn _ _ [] = []
gn g r x = let w = [n | n<-x, n!!(length n-1)==r]in if length w > 0 then w else gn g r (helper g x)
helper :: Graph -> [[Int]] -> [[Int]]
helper _ [] = []
helper g (a:as) = let adj = g!!(a!!(length a - 1))in [a++[x] | x<-adj, not (elem x a)] ++ helper g as
ad :: Int -> [Int] -> Int
ad _ [] = 0
ad x (y:ys) | x==y = 1+(ad x ys)| otherwise = ad x ys
nhelp :: Graph -> [Int]
nhelp grap = [0..(length grap - 1)]
hw :: Graph -> Int -> Int -> Bool
hw g r w = [] /= shortWay g r w
gh1::(Maybe [Int],Int,Int) -> Bool
gh1 (xs,i,resPred) = case xs of
    Just ys -> (resPred <=0)||((last ys) >=i)
    Nothing -> False

dostep :: System -> Recur -> (Maybe [Int],Int,Int)-> (Maybe [Int],Int,Int)
dostep syst r (ys,i,predRes) = case ys of
    Just xs -> case evalPart syst r (init xs++[last xs+1]) of
        Just res-> (Just (init xs++[last xs+1]),i,res) 
        Nothing -> (Nothing,i,predRes)
    Nothing -> (Nothing,i,predRes)

tyr::(Maybe [Int],Int)->Bool
tyr (xs,i)= case xs of 
    Just ys -> i <= (ys !! (length ys -2))
    Nothing -> True

wrytefy::System -> Recur -> (Maybe [Int],Int) -> (Maybe [Int],Int)
wrytefy syst r1 (ys,i)= case ys of
    Just xs -> let without1 = init xs
                   counter = (last without1)
                   without2 = init without1
               in case evalPart syst r1 xs of
                       Just res -> (Just (without2 ++ [counter+1]++[res]),i)
                       Nothing -> (Nothing,i)
    Nothing -> (Nothing, i)
    

pre:: System -> [Recur]-> [Int] -> [Int]-> Maybe [Int]
pre syst (y:rest) xs res= case evalPart syst y xs of
     Just i -> pre syst rest xs (res++[i])
     Nothing -> Nothing
pre _ [] _ res = Just res
delhelp :: (Ord a) => BinTreeM a -> BinTreeM a
delhelp (NodeM _ _ l  EmptyM) = l
delhelp (NodeM _ _ EmptyM  d) = d
delhelp (NodeM _ b c d) = NodeM (leftEl d) b c d

remove :: String -> Char -> String
remove [] _ = []
remove (x:ss) a | x==a = remove ss a| otherwise = [x]++(remove ss a)
leftEl :: (Ord a) => BinTreeM a -> a
leftEl (NodeM a _ EmptyM  _) = a
leftEl (NodeM _ _ c _) = leftEl c
helpsort::(Ord a)=> BinTreeM a -> [a]
helpsort EmptyM = []
helpsort (NodeM a b c d ) = helpsort c ++(replicate b a)++helpsort d
---------------------Тестові дані - Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

---------------------Тестові дані - Рекурсивні функції -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]