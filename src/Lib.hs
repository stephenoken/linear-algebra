module Lib
    (Vector,
     plus,
     subtract,
     scalarM
    ) where
import Prelude hiding (subtract)
import Data.Decimal

-- Data Types
data Vector = Vector [Decimal]
 
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

scalarM ::Decimal -> Vector -> Vector
scalarM a (Vector x) = Vector $ map (* a) x 
