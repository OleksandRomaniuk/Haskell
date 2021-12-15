{-# OPTIONS_GHC -Wall #-}
module ROMANIUK02 where

-- Завдання 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs
  
-- Завдання 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- Завдання 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = if null xs then ys else
 (if null ys then xs else foldr (:) ys xs)

-- Завдання 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert  = foldl myInsert []
myInsert :: [Integer] -> Integer -> [Integer]
myInsert [] x = [x]
myInsert xs v
  | v < (head xs) = v : (head xs) : (tail xs)
  | otherwise = (head xs) : myInsert (tail xs) v 

-- Завдання 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f xs ys = f (head xs) (head ys) : map2 f (tail xs) (tail ys) 

-- Завдання 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum ([fromIntegral (m ^ i) / fromIntegral (factorial i) | i <- [1..n]])


-- Завдання 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Завдання 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x*x | x <- [1..]]


-- Завдання 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = assistIndexes 0 xs ys

assistIndexes :: Int -> [Int] -> [Int] -> [Int]
assistIndexes n [] [] = [n]
assistIndexes n [] ys =  n: (assistIndexes (n+1) [] (tail ys))
assistIndexes _ _ [] = []
assistIndexes n xs ys = if head xs /= head ys then assistIndexes (n+1) xs (tail ys) else
 (if contains xs ys then n : assistIndexes (n+1) xs (tail ys) else assistIndexes (n+1) xs (tail ys))

contains :: [Int] -> [Int] -> Bool
contains [] _ = True
contains _ [] = False
contains xs ys = if head xs /= head ys then False else contains (tail xs) (tail ys)