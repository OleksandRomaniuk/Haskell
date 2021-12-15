{-# OPTIONS_GHC -Wall #-}
module HWP01 where

-- ������ 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n  = if n < 1 then 1 else n * factorial (n - 1)

-- ������ 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if (null xs) && (null ys) then [] else
       if (null xs) && (not (null ys)) then (0 + (head ys)) : listSum [] (tail ys) else 
             if (null ys) && (not (null xs)) then ((head xs) + 0) : listSum (tail xs) [] else
                    ((head xs) + (head ys)) : listSum (tail xs) (tail ys)

-- ������ 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven xs = if null xs then [] else 
 if null (tail xs) then xs else 
  (head(tail xs) : (head xs : oddEven (tail (tail xs))))

-- ������ 4 -----------------------------------------
positionAsist :: Int -> Int -> [Int] -> Int
positionAsist n m xs = if null xs then -1 else 
 (if head xs == n then m else positionAsist n (m+1) (tail xs))

position    ::  Int -> [Int] -> Int
position n xs = positionAsist n 0 xs  

-- ������ 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = if null xs then [] else (head xs) : set (filter1 ((head xs) /=) (tail xs))
memb :: Int -> [Int] -> Bool
memb x ys = if null ys then False else if ((head ys) == x) then True else memb x (tail ys)
filter1 :: (Int -> Bool) -> [Int] -> [Int]
filter1 p xs = if null xs then [] else 
                 if  p (head xs) then (head xs) : filter1 p (tail xs) 
                                 else filter1 p (tail xs)


-- ������ 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set(concatMy xs ys) 

concatMy :: [Int] -> [Int] -> [Int]
concatMy xs ys = if null xs then ys else
 (if null ys then xs else (head xs) : concatMy (tail xs) ys)
-- ������ 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = if (null xs) || (null ys) then [] else
       set (if memb (head xs) ys then (head xs) : (intersection (tail xs) ys) else
       (intersection (tail xs) ys))

-- ������ 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]]