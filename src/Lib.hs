module Lib
    (Vector,
     plus,
     subtract,
     scalarM,
     magnitude,
     normalise
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
