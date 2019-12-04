
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Lib

import Test.Hspec

genWalkCase :: String -> [Point] -> Spec
genWalkCase input expected = context ("walk path " ++ show input) $ do
  it ("will produce points " ++ show expected) $ do
    touched (walkPath input) `shouldMatchList` expected 

genIntersecCase :: [Point] -> [Point] -> [Point] -> Spec
genIntersecCase pointsA pointsB expected =
  context (showString "intersections between "
           . shows pointsA
           . showString " and "
           . show $ pointsB ) $ do
    it ("will have intersections " ++ show expected) $ do
      intersections [pointsA, pointsB] `shouldMatchList` expected

main :: IO ()
main = hspec $ do
  describe "walking paths" $ do
    genWalkCase "U1, R1, U1" [(Point 0 1),
                              (Point 1 1),
                              (Point 1 2)]

    genWalkCase "D1, L1, D1" [(Point 0 (-1)),
                              (Point (-1) (-1)),
                              (Point (-1) (-2))]

    genWalkCase "R5, U1, L3, D2" [(Point 1  0),
                                  (Point 2  0),
                                  (Point 3  0),
                                  (Point 4  0),
                                  (Point 5  0),
                                  (Point 5  1),
                                  (Point 4  1),
                                  (Point 3  1),
                                  (Point 2  1),
                                  (Point 2  0),
                                  (Point 2 (-1))]

  describe "path intersections" $ do
    genIntersecCase [(Point 0 4)] [(Point 1 0)] [] 
    genIntersecCase [(Point 1 1)] [(Point 1 1)] [(Point 1 1)] 
    genIntersecCase [(Point 0 4), (Point 5 7)] [Point 3 1, Point 5 7] [Point 5 7] 
