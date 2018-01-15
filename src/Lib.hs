module Lib
    ( someFunc,
      add5
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add5 :: (Num a) => a -> a
add5 = (+) 10
