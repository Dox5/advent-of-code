import qualified TestTape as TestTape
import qualified TestElfScore as TestElfScore

import Control.Monad(foldM)

import System.Exit (exitFailure)

allSuites :: [IO Bool]
allSuites = [TestTape.runTests, TestElfScore.runTests]

main :: IO ()
main = do
  result <- (sequence allSuites >>= foldM (\x y -> return $ x && y) True)
  if result then
    putStrLn "All tests passed"
  else
    putStrLn "Some tests failed" >> exitFailure
