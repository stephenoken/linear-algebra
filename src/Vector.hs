module Vector
    (Vector (..),
     plus,
     subtract,
     scalarM,
     magnitude,
     normalise,
     dot,
     angleInRadians,
     isParallel,
     isOrthogonal
    ) where
import Prelude hiding (subtract)

-- Data Types
data Vector = Vector [Float]

instance Eq Vector where
  Vector x == Vector y = x == y

instance Show Vector where
  show (Vector x) = "V" ++ show x

instance Monoid Vector where
  mempty = Vector []
  mappend = plus

plus :: Vector -> Vector -> Vector
plus (Vector x) (Vector y) = Vector $ zipWith (+) x y

subtract :: Vector -> Vector -> Vector
subtract (Vector x) (Vector y) = Vector $ zipWith (-) x y

scalarM :: Float -> Vector -> Vector
scalarM a (Vector x) = Vector $ map (* a) x

magnitude :: Vector -> Float
magnitude (Vector xs) = sqrt $ foldl sumSq 0.0 xs

sumSq :: Float -> Float -> Float
sumSq prev curr = prev + curr ^ 2

normalise :: Vector -> Vector
normalise v  = (1.0 / magnitude v) `scalarM` v

dot :: Vector -> Vector -> Float
dot (Vector xs) (Vector ys) = sum $ zipWith (*) xs ys

angleInRadians :: Vector -> Vector -> Float -- θ
angleInRadians v1 v2 = acos $ (v1 `dot` v2) / (magnitude v1 * magnitude v2)

angleInDegrees :: Vector -> Vector -> Float
angleInDegrees v1 v2 = (180.0 / pi) * (angleInRadians v1 v2)

percisionRound :: (RealFrac a, Integral p) => a -> p -> a
percisionRound x p = fromInteger (round (x * factor)) / factor
  where factor = 10^p

isParallel :: Vector -> Vector -> Bool --super percise :(
isParallel v1 v2
  | angle == 0.0 || angle == percisionRound pi 3 = True
  | otherwise = False
  where angle = percisionRound (v1 `angleInRadians` v2) 3

isOrthogonal :: Vector -> Vector -> Bool
isOrthogonal v1 v2
  | percisionRound (abs $ dot v1 v2) 3 < 1e-10 = True
  | otherwise = False

proj :: Vector -> Vector -> Vector
proj v b = ( v `dot` normalisedB ) `scalarM` normalisedB
  where normalisedB = normalise b

findOrth :: Vector -> Vector -> Vector
findOrth v b = v `subtract` (proj b v)

crossProd :: Vector -> Vector -> Vector
crossProd (Vector [x1, y1, z1]) (Vector [x2, y2, z2]) = Vector [x', y', z']
  where
    x' = (y1 * z2) - (y2 * z1)
    y' = -((x1 * z2) - (x2 * z1))
    z' = (x1 * y2) - (x2 * y1)

parallelogramArea :: Vector -> Vector -> Float
parallelogramArea v1 v2 = magnitude $ crossProd v1 v2

triangleArea :: Vector -> Vector -> Float
triangleArea v1 v2 = 0.5 * ( magnitude $ crossProd v1 v2)
