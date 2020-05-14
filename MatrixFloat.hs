module MatrixFloat
( Matrix, Row, size
, det, invertible
, orthogonal, timesS
, plusM, timesM, dot
, idM, refproto, showM
) where

import Data.List

type Matrix = [Row]
type Row = [Float]

size :: Matrix -> (Int, Int)
size m = let row = length m
             col = head (map length m)
         in (row, col)

det :: Matrix -> Float
det m
  | condition2x2 m = let ls = concat m
                     in (ls !! 0 * ls !! 3) - (ls !! 1 * ls !! 2)
  | otherwise = let (x:xs) = transpose m
                    nums = zip [0..] x
                    in sum [((-1)^n) * a * det (dropnMap n xs) |(n,a)<-nums]

dropnMap :: Int -> [[a]] -> [[a]]
dropnMap 0 m = map (drop 1) m
dropnMap n m = map (\ xs -> (take n xs) ++ (drop (n+1) xs)) m

condition2x2 :: Matrix -> Bool
condition2x2 m = (fst (size m) == 2) && (snd (size m) == 2)

invertible :: Matrix -> Bool
invertible m = (det m /= 0) && (fst (size m) == snd (size m))

orthogonal :: Matrix -> Bool
orthogonal m = let m1 = transpose m
               in allzero [[c]`dot`[c'] |c<-m1, c'<-m1, c/=c']
                  &&
                  allone [[c]`dot`[c'] |c<-m1, c'<-m1, c==c']

timesS :: Float -> Matrix -> Matrix
timesS s = map (map (*s))

valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all (== head xs) (tail xs)

plusM :: Matrix -> Matrix -> Matrix
plusM m n | ok        = zipWith (zipWith (+)) m n
          | otherwise = error "Invalid input matrices"
  where ok = valid m && valid n && size m == size n


timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | ok        = [ [ dot row col | col <- transpose m2 ] | row <- m1 ]
             | otherwise = error "Invalid input matrices."
  where dot xs ys = sum (zipWith (*) xs ys)
        ok        = snd(size m1) == fst (size m2)

dot :: Matrix -> Matrix  -> Float
dot m1 m2 | (size m1 == size m2) = sum $ concat (zipWith (zipWith (*)) m1 m2)
          | otherwise = error "Invalid input matrices."

idM :: Int -> Matrix
idM 0 = []
idM 1 = [[1]]
idM n = (1 : replicate (n-1) 0) : (map (0:) (idM (n-1)))

refproto :: Matrix -> Matrix
refproto [] = []
refproto [[x]] = [[x]]
refproto m =  let rown = fst (size m)
                  cols = zip [0..] (map head m)
                  pivotrow = fst $ head [x |x<-cols, snd x /= 0]
                  mf = ero rown (takeRup m pivotrow)
                  row1 = head mf
                  col1 = head $ transpose (tail mf)
                  mnext = map tail (tail mf)
              in row1:(transpose $ col1 : (transpose (refproto mnext)))

elimination2 :: Matrix -> Matrix
elimination2 m = let col1 = zip [0..] (map head m)
                     pivotrow = fst $ head [x |x<-col1, snd x /= 0]
                     m2 = takeRup m pivotrow
                  in ero 2 m2

ero :: Int -> Matrix -> Matrix
ero n m@(x:xs)
  | allzero (map head xs) = m
  | otherwise = let p = x
                    t = m!!(n-1)
                    c = head t / head p
                    m2 = (take (n-1) m) ++ [t `subtractR`(scaleR p c)] ++ (drop (n) m)
                in ero (n-1) m2

allzero :: Row -> Bool
allzero r = and [x == 0 |x<-r]

allone :: Row -> Bool
allone r = and [x == 1 |x<-r]

subtractR :: Row -> Row -> Row
subtractR r1 r2 = zipWith (-) r1 r2

scaleR :: Row -> Float -> Row
scaleR r n = map (*n) r

takeRup :: Matrix -> Int -> Matrix
takeRup m n = let row =  m!!(n) in row : [x |x<-m, x/=row]

diagonal :: Matrix -> Int -> [Float]
diagonal [] _ = []
diagonal _ 0 = []
diagonal m n = (map (!!(n-1)) m)!!(n-1) : diagonal m (n-1)

showM :: Matrix -> IO ()
showM [x] = print x
showM (x:xs) = do
  print x
  showM xs
