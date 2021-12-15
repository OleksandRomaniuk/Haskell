{-# OPTIONS_GHC -Wall #-}
module ROMANIUK04M where

import Data.List(nub,sort)

-- машина Тюрінга
data Going = No | Lt | Rt deriving (Show,Eq,Ord)
type Table = [((Int,Char),(Int,Char,Going))]
type Machine = (Int, Table)
type Config = (String,(Int,Char), String, Int)

-- опис складної машини Тюрінга 
-- L | R | P Char | G  базові машини
data Complex = Join [Complex] 
             | While Char Complex 
             | If Char Complex Complex 
             | P Char 
             | L | R | G 
             deriving (Show, Eq)


-- Задача 1.b ----------------------------------------- 
states :: Machine -> [Int]
states m = tail $ nub $ sort ([fst (fst x)| x <- snd m] ++ [fst3 (snd x)| x <- snd m])


alphabet :: Table -> [Char]
alphabet t = nub $ sort ([snd (fst x)| x <- t] ++ [funk3 (snd x)| x <- t])

funk3 :: (a, b, c) -> b
funk3 (_, x, _) = x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Задача 2 -----------------------------------------
iswfMachine :: Machine -> Bool
dop :: Machine -> Int 
check :: Machine -> Bool
funct :: [Int] -> [Char] -> [(Int,Char)]
finalcheck :: Machine -> [(Int,Char)]
listcheck :: [(Int,Char)] -> [(Int,Char)] -> Bool

iswfMachine x = listcheck (funct (states x) (alphabet (snd x))) (finalcheck x)
dop ax = fst(ax)
check ax = if length (states(ax)) == fst(ax) then True else False
funct sa sb = [(a,b) | a <- sa, b <- sb]
finalcheck xs = let sec = snd(xs) in map (\x -> fst x) sec
listcheck xs ys = if (sort(xs) == sort(ys)) then True else False

-- Задача 3.a -----------------------------------------
isFinal ::  Int -> Config -> Bool
isFinal = undefined

-- Задача 3.b -----------------------------------------
stepM :: Machine -> Config -> Config
stepM = undefined 

-- Задача 4 -----------------------------------------
eval :: Machine -> Int -> String -> Maybe String
eval = undefined

-- Задача 5.a -----------------------------------------
renum :: Int -> Machine -> Machine   
renum = undefined

-- Задача 5.b -----------------------------------------
connect :: Int -> Table -> Table
connect = undefined

-- Задача 6.a -----------------------------------------
seqJoin :: Machine -> Machine -> Machine 
seqJoin = undefined 

-- Задача 6.b -----------------------------------------
ifJoin :: Char -> String -> Machine -> Machine -> Machine
ifJoin = undefined

-- Задача 6.c -----------------------------------------
cycleJoin :: Char -> String -> Machine -> Machine 
cycleJoin = undefined

-- Задача 7 -----------------------------------------
build ::  String -> Complex -> Machine
build = undefined     

-- Задача 8.a-----------------------------------------
subtractAbs :: Complex
subtractAbs = undefined

-- Задача 8.b-----------------------------------------
subtraction :: Complex     
subtraction = undefined

--------------------------------------------------------
--  тестові дані 
-- приклади машин Тюрінга 
test1, test2 :: Machine 
-- алфавіт " abc": знаходить перший символ 'a' заміняє на 'b' і зупиняється  
test1 = (1, [ ((1,'a'),(0,'b',No)), ((1,'b'),(1,'b',Rt))
            , ((1,'c'),(1,'c',Rt)), ((1,' '),(1,' ',Rt))])
-- алфавіт " a": невизначена функція переходу в (1,'a') !!! 
test2 = (2,[((2,'a'),(2,'a',Rt)),((2,' '),(1,' ',Rt)),((1,' '),(0,' ',Rt))])

-- будуємо складну машину з найпростіших
-- будуємо машину, що обчислює додавання
-- найпростіші . алфавіт == " #|"
rht, putO, putW :: Machine 
rht  = (1, map (\c->((1,c),(0,c,Rt))) " #|")   -- переміщує вправо
putO = (1, map (\c->((1,c),(0,'|',No))) " #|") -- записує символ '|'
putW = (1, map (\c->((1,c),(0,' ',No))) " #|") -- записує символ ' '

-- складніші машини 
rightO, rightM, main, additionM :: Machine 
rightO = cycleJoin '|' " #|" rht      -- проходить вправо всі '|'
rightM = seqJoin rht rightO           -- вправо завжди і потім вправо всі '|' 
main   = seqJoin (seqJoin putW rightM) putO  -- додавання, коли x>0
additionM = ifJoin '|' " #|" main putW       -- додавання, коли x>=0 

-- приклади побудов машин Тюрінга (обєкти типу Complex)
right, left, copy, addition :: Complex 
-- вправо завжди і потім вправо всі '|'
right = Join [R,While '|' R]
-- вліво завжди і потім вліво всі '|'  
left  = Join [L,While '|' L] 
-- додавання x+y 
addition = If '|' (Join [P ' ',right,P '|']) (P ' ')  
-- копіювання *|.x.| |.y.| ==> *|.x.| |.y+x.| 
copy = Join [While '|' (Join [P ' ',right,right,P '|',left,left,P '|',R])
            ,Join [left,R]
            ]

rightOT, rightMT, mainT, additionMT :: Machine 
rightOT = (2,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))])
rightMT = (3,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))
  ,((3,' '),(2,' ',Rt)),((3,'#'),(2,'#',Rt)),((3,'|'),(2,'|',Rt))])
mainT = (5,
  [((1,' '),(0,'|',No)),((1,'#'),(0,'|',No)),((1,'|'),(0,'|',No))
  ,((2,' '),(3,' ',Rt)),((2,'#'),(3,'#',Rt)),((2,'|'),(3,'|',Rt))
  ,((3,' '),(1,' ',No)),((3,'#'),(1,'#',No)),((3,'|'),(2,'|',No))
  ,((4,' '),(3,' ',Rt)),((4,'#'),(3,'#',Rt)),((4,'|'),(3,'|',Rt))
  ,((5,' '),(4,' ',No)),((5,'#'),(4,' ',No)),((5,'|'),(4,' ',No))])  
additionMT = (7,
  [((1,' '),(0,' ',No)),((1,'#'),(0,' ',No)),((1,'|'),(0,' ',No))
  ,((2,' '),(0,'|',No)),((2,'#'),(0,'|',No)),((2,'|'),(0,'|',No))
  ,((3,' '),(4,' ',Rt)),((3,'#'),(4,'#',Rt)),((3,'|'),(4,'|',Rt))
  ,((4,' '),(2,' ',No)),((4,'#'),(2,'#',No)),((4,'|'),(3,'|',No))
  ,((5,' '),(4,' ',Rt)),((5,'#'),(4,'#',Rt)),((5,'|'),(4,'|',Rt))
  ,((6,' '),(5,' ',No)),((6,'#'),(5,' ',No)),((6,'|'),(5,' ',No))
  ,((7,' '),(1,' ',No)),((7,'#'),(1,'#',No)),((7,'|'),(6,'|',No))])