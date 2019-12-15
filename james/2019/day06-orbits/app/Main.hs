module Main where

import Lib

import System.IO as IO

loadMap :: IO.FilePath -> IO OrbitMap
loadMap path = do
  contents <- IO.readFile path
  return . load $ contents

main :: IO ()
main = do
  m <- loadMap "input.txt"
  putStrLn . showString "Checksum: " . show . checksum $ m
  putStrLn . showString "YOU -> SAN: " . show . distBetween m "YOU" $ "SAN"
