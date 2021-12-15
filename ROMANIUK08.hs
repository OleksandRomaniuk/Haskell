{-# OPTIONS_GHC -Wall #-}
module ROMANIUK08 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 -----------------------------------------			   
isSearch :: (Ord a) => BinTree a -> Bool
isSearch EmptyB = True
isSearch (Node x tll trr) = check LT x tll && check GT x trr
    where
        check _ _ EmptyB = True
        check comp x (Node y tll trr) = (comp == compare y x) && check LT y tll && check GT y trr

-- Задача 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch (Node p x y) v
        | v == p = True
        | v > p = elemSearch y v
        | otherwise = elemSearch x v

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB y = Node y EmptyB EmptyB
insSearch t@(Node v tll trr) y | not$isSearch t = error "Must be searsh in tree"
                             | y>v = (Node v tll (insSearch trr y))
                             | y<v = (Node v (insSearch tll y) trr)
                             | otherwise = error "Unex value"

-- Задача 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB _ = EmptyB
delSearch (Node h tr EmptyB) v = if (v==h) then tr else if (v<h) then (Node h (delSearch tr v) EmptyB) else (Node h tr EmptyB)
delSearch (Node h EmptyB tl) v = if (v==h) then tl else if (v<h) then (Node h EmptyB tl) else (Node h EmptyB (delSearch tl v))
delSearch (Node h t1 t2) v = if(v < h) then (Node h (delSearch t1 v) t2) else if (v > h) then (Node h t1 (delSearch t2 v)) else (Node m t1 (delSearch t2 m)) where m = findElem t2

-- Задача 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList a = createList (foldl (insSearch) EmptyB a)

-- Задача 6-----------------------------------------

isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 (Empty23) = True
isTree23 (Leaf _) = True
isTree23 (Node2 lt k rt) = checkTree (Node2 lt k rt) && (k == (minTr rt))
isTree23 (Node3 lt k1 mt k2 rt) = checkTree (Node3 lt k1 mt k2 rt) && (k1 == (minTr mt)) &&(k2 == (minTr rt))


checkTree :: (Ord a) => Tree23 a -> Bool
checkTree (Leaf _) = True
checkTree Empty23 = True
checkTree (Node2 lt _ rt)
    | (func1 lt) && not (func1 rt) = False
    | not (func1 lt) && (func1 rt) = False
    | (func1 lt) && (func1 rt) = True
    | otherwise = (checkTree lt) && (checkTree rt)
checkTree (Node3 lt _ mt _ rt)
    | not (func1 lt) && (func1 mt) && (func1 rt) = False
    | (func1 lt) && not (func1 mt) && (func1 rt) = False
    | (func1 lt) && (func1 mt) && not (func1 rt) = False
    | (func1 lt) && (func1 mt) && (func1 rt) = True
    | otherwise = (checkTree lt) && (checkTree mt) && (checkTree rt)

func1 :: (Ord a) => Tree23 a -> Bool
func1 (Leaf _) = True
func1 _ = False

minTr :: (Ord a) => Tree23 a -> a
minTr (Leaf v) = v
minTr (Empty23) = error "minInRightTree"
minTr (Node2 lt _ _) = minTr lt
minTr (Node3 lt _ _ _ _) = minTr lt


-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Empty23) _ = False
elemTree23 (Leaf x) y = if (x == y) then True else False
elemTree23 (Node2 s1 p s2) v = 
        case compare v p of
                EQ -> True
                GT -> elemTree23 s2 v
                LT -> elemTree23 s1 v
elemTree23 (Node3 s1 p1 s2 p2 s3) v = 
        case (compare v p1, compare v p2) of
                (EQ, _) -> True
                (_, EQ) -> True
                (_, GT) -> elemTree23 s3 v
                (LT, _) -> elemTree23 s1 v
                (_, LT) -> elemTree23 s2 v

-- Задача 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = (conv t1) == (conv t2)

conv :: (Ord a) => Tree23 a -> [a]
conv Empty23 = []
conv (Leaf x) = [x]
conv (Node2 tl _ tr) = (conv tl)++(conv tr)
conv (Node3 t1 _ t2 _ t3) = (conv t1)++(conv t2)++(conv t3)

-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 (Empty23) v = (Leaf v)
insTree23 (Leaf l) v =
        case compare l v of
                EQ -> (Leaf l)
                LT -> Node2 (Leaf l) v (Leaf v)
                GT -> Node2 (Leaf v) l (Leaf l)
insTree23 (Node2 s1 p s2) v = 
        case compare p v of 
                EQ -> (Node2 s1 v s2)
                LT -> fun2 s1 p (insTree23 s2 v)
                GT -> fun2 (insTree23 s1 v) p s2 
insTree23 (Node3 s1 p1 s2 p2 s3) v = 
        case (compare v p1, compare v p2) of
                (EQ, _) -> Node3 s1 p1 s2 p2 s3
                (_, EQ) -> Node3 s1 p1 s2 p2 s3
                (_, GT) -> fun3 s1 p1 s2 p2 (insTree23 s3 v)
                (LT, _) -> fun3 (insTree23 s1 v) p1 s2 p2 s3
                (_, LT) -> fun3 s1 p1 (insTree23 s2 v) p2 s3

fun2 :: (Ord a) => Tree23 a -> a -> Tree23 a -> Tree23 a 
fun2 (Leaf l1) p (Leaf l2) = Node2 (Leaf l1) p (Leaf l2)
fun2 (Node2 s1 p1 s2) p2 s3 = Node3 s1 p1 s2 p2 s3
fun2 s1 p1 (Node2 s2 p2 s3) = Node3 s1 p1 s2 p2 s3
fun2 s1 p s2 = Node2 s1 p s2
fun3 :: (Ord a) => Tree23 a -> a -> Tree23 a -> a -> Tree23 a -> Tree23 a
fun3 (Leaf l1) p1 (Leaf l2) p2 (Leaf l3) = Node3 (Leaf l1) p1 (Leaf l2) p2 (Leaf l3)
fun3 s1 p1 s2 p2 (Node2 s3 p3 s4) = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
fun3 s1 p1 (Node2 s2 p2 s3) p3 s4 = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
fun3 (Node2 s1 p1 s2) p2 s3 p3 s4 = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
fun3 s1 p1 s2 p2 s3 = Node3 s1 p1 s2 p2 s3


findElem :: (Ord a) => BinTree a -> a
findElem EmptyB = error "EMPTY TREE"
findElem (Node h EmptyB _) = h
findElem (Node _ t1 _) = findElem t1
createList :: (Ord a) => BinTree a -> [a]
createList EmptyB = []
createList (Node v lt rt) = createList(lt) ++ [v] ++ createList(rt)

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  Бінарні дерева 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )