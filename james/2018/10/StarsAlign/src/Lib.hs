module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point a = Point a a

data Sky = Sky [Point Int]
