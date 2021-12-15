{-# OPTIONS_GHC -Wall #-}
module ROMANIUK06 where

newtype Poly a = P [a]

-- ������ 1 -----------------------------------------
x :: Num a => Poly a
x = P[0,1]

-- ������ 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = zeros p1 == zeros p2
        where zeros = dropWhile (==0) . reverse
 
-- ������ 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P coefs) =
    if all (== 0) coefs
      then "0"
      else concatWithPlus $ formattedCoefs (reverse coefs)
    where
      formattedCoefs [] = []
      formattedCoefs (d:ds) = (showTerm d (length ds)) ++ formattedCoefs ds
      showTerm c e
        | c == 0 = []
        | c == 1 && e == 0 = ["1"]
        | otherwise = [showCoef c ++ showExp e]
      showCoef c
        | c == 1 = ""
        | c == -1 = "-"
        | otherwise = show c
      showExp e
        | e == 1 = "x"
        | e == 0 = ""
        | otherwise = "x^" ++ show e
      concatWithPlus [] = []
      concatWithPlus (term:[]) = term ++ concatWithPlus []
      concatWithPlus (term1:term2:[]) =
        term1 ++ " + " ++ term2 ++ concatWithPlus []
      concatWithPlus (term:terms) = term ++ " + " ++ concatWithPlus terms

-- ������ 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P[]) (P[]) = P[]
plus p (P[]) = p
plus (P[]) p = p
plus (P(zs)) (P(ys)) | length zs > length ys = P([(zs!!n) + (ys!!n) | n <- [0..(length ys -1)]] ++ [zs!!m | m <-[(length ys)..(length zs-1)]])
                     | length zs < length ys = P([(zs!!n) + (ys!!n) | n <- [0..(length zs -1)]] ++ [ys!!m | m <-[(length zs)..(length ys-1)]])
                     | otherwise = P[(zs!!n) + (ys!!n) | n <- [0..(length zs -1)]]



-- ������ 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum $ f 0 a b
    where
        f :: (Num a) => Int -> [a] -> [a] -> [Poly a] 
        f _ [] _ = [0]
        f c [aa] bs = [item c aa bs]
        f c (aa:as) bs = item c aa bs : f (c + 1) as bs
        item c aa bs = P $ replicate c 0 ++ map (*aa) bs

-- ������ 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P (map negate a)
    fromInteger n = P [fromInteger n]

    abs    = undefined
    signum = undefined


-- ������ 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P[]) _ = 0
applyP (P(zs)) n = foldl1 (+) [(zs!!m)*(n^m) | m <- [0..(length zs -1)]]

-- ������ 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 y = deriv y
    nderiv n y = deriv(nderiv (n-1) y)


-- ������ 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P xs) = P (zipWith (\i n -> fromInteger i * n) [1..] (drop 1 xs))

