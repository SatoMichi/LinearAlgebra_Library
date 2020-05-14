module Matrix
( Matrix
, Row
, size
, det
, invertible
, timesS
, plusM
, timesM
, dot
, idM
) where

import Data.List

type Matrix a = [Row a]
type Row a = [a]

size :: Matrix a -> (Int, Int)
size m = let row = length m
             col = head (map length m)
         in (row, col)

det :: Matrix Int -> Int
det m
  | condition2x2 m = let ls = concat m
                     in (ls !! 0 * ls !! 3) - (ls !! 1 * ls !! 2)
  | otherwise = let (x:xs) = transpose m
                    nums = zip [0..] x
                    in sum [((-1)^n) * a * det (dropnMap n xs) |(n,a)<-nums]

dropnMap :: Int -> [[a]] -> [[a]]
dropnMap 0 m = map (drop 1) m
dropnMap n m = map (\ xs -> (take n xs) ++ (drop (n+1) xs)) m

condition2x2 :: Matrix a -> Bool
condition2x2 m = (fst (size m) == 2) && (snd (size m) == 2)

invertible :: Matrix Int -> Bool
invertible m = (det m /= 0) && (fst (size m) == snd (size m))

timesS :: Int -> Matrix Int -> Matrix Int
timesS s = map (map (*s))

valid :: Matrix Int -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all (== head xs) (tail xs)

plusM :: Matrix Int -> Matrix Int -> Matrix Int
plusM m n | ok        = zipWith (zipWith (+)) m n
          | otherwise = error "Invalid input matrices"
  where ok = valid m && valid n && size m == size n


timesM :: Matrix Int -> Matrix Int -> Matrix Int
timesM m1 m2 | ok        = [ [ dotv row col | col <- transpose m2 ] | row <- m1 ]
             | otherwise = error "Invalid input matrices."
  where dotv xs ys = sum (zipWith (*) xs ys)
        ok        = snd(size m1) == fst (size m2)

dot :: Matrix Int -> Matrix Int -> Int
dot m1 m2 | (size m1 == size m2) = sum $ concat (zipWith (zipWith (*)) m1 m2)
          | otherwise = error "Invalid input matrices."

idM :: Int -> Matrix Int
idM 0 = []
idM 1 = [[1]]
idM n = (1 : replicate (n-1) 0) : (map (0:) (idM (n-1)))

elimination :: Matrix Int -> Matrix Int
elimination [] = []
elimination m@(x:xs) = let dim = snd (size m)
                           dia = diagonal m dim
                           mij' = undefined
                       in undefined

diagonal :: Matrix Int -> Int -> [Int]
diagonal [] _ = []
diagonal _ 0 = []
diagonal m n = (map (!!(n-1)) m)!!(n-1) : diagonal m (n-1)
