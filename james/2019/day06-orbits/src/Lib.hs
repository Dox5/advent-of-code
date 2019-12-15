module Lib where

import Data.Graph (Graph)
import qualified Data.Graph as Graph

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

import Data.Maybe (isJust)

import qualified Debug.Trace as D

data OrbitMap = OrbitMap  {
  orbitMap :: Graph,
  idToName :: IntMap String,
  nameToId :: HashMap String Int
}
  deriving(Show)

bodyCode :: ReadP (String)
bodyCode = do
  let symbol = ReadP.satisfy (\c -> any (c==) "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  ReadP.many1 symbol

-- LHS has object RHS orbiting it
orbitDeclaration :: ReadP (String, String)
orbitDeclaration = do
  lhsCode <- bodyCode
  _ <- ReadP.char ')'
  rhsCode <- bodyCode
  _ <- ReadP.char '\n'
  return (lhsCode, rhsCode)

orbits :: ReadP [(String, String)]
orbits = do
  codes <- ReadP.many orbitDeclaration
  ReadP.eof
  return codes

uniqueBodies :: [(String, String)] -> [String]
uniqueBodies = Set.toList . Set.fromList . flatten . unzip
    where
      flatten :: ([a], [a]) -> [a]
      flatten (x, y) = concat [x, y]

buildOrbitGraph :: HashMap String Int -> [(String, String)] -> Graph
buildOrbitGraph mapping = Graph.buildG (0, (HashMap.size mapping)-1). fmap conv2
  where
    conv :: String -> Int
    conv s = mapping HashMap.! s
    conv2 :: (String, String) -> (Int, Int)
    conv2 (a, b) = (conv a, conv b)

load :: String -> OrbitMap
load spec =
  let
    ((parsed, ""):[]) = ReadP.readP_to_S orbits spec
    bodies = uniqueBodies parsed
    idedBodies = zip [0..] bodies
    nToI = HashMap.fromList . fmap Tuple.swap $ idedBodies 
  in OrbitMap {
    orbitMap = buildOrbitGraph nToI parsed,
    idToName = IntMap.fromList idedBodies,
    nameToId = nToI
  }

checksum :: OrbitMap -> Int
checksum m =
  case  Graph.dfs (orbitMap m) [((nameToId m) HashMap.! "COM")] of
    (n:[]) -> recurse 0 n
    _ -> error "Must be single root!"
  where
    recurse :: Int -> Graph.Tree Graph.Vertex -> Int
    recurse depth v =
      case v of
        -- This isn't great and is probably why the algorithm is slow when it 
        -- is too slow (or runs out of memory!)
        (Graph.Node _ sub) -> List.foldl' (\t n -> t + (recurse (depth+1) n)) depth sub

type OrbitTree = Graph.Tree Graph.Vertex

keepJust :: Maybe a -> Maybe a -> Maybe a
keepJust (Just x) Nothing = Just x
keepJust _ accum = accum

keepJust3 :: (Maybe a, Maybe a, Maybe a) -> (Maybe a, Maybe a, Maybe a) -> (Maybe a, Maybe a, Maybe a)
keepJust3 (n1, n2, n3) (o1, o2, o3) = ((keepJust n1 o1), (keepJust n2 o2), (keepJust n3 o3))

calcDist :: (Maybe Int, Maybe Int, Maybe Int) -> Maybe Int
calcDist t@((Just a), (Just b), (Just common)) = D.trace (show t) $ Just ((a - 1) + (b - 1) - (common*2))
calcDist _ = Nothing


travelDistance :: OrbitTree -> Graph.Vertex -> Graph.Vertex -> Maybe Int
travelDistance root lhs rhs = calcDist . decent 0 $ root


  where
    decent :: Int -> OrbitTree -> (Maybe Int, Maybe Int, Maybe Int)
    decent depth (Graph.Node vert sub) = 
      D.trace ("depth" ++ (show depth) ++ "node:" ++ show vert) $
      let
        (foundL, foundR, lowerAnc) = foldr keepJust3 (Nothing, Nothing, Nothing) . fmap (decent (depth + 1)) $ sub
        (thisL, thisR) =
          if vert == lhs then
            (Just depth, Nothing) 
          else if vert == rhs then
            (Nothing, Just depth)
          else
            (Nothing, Nothing)

        ancestor = if (isJust foundL) && (isJust foundR) ||
                        (isJust foundL) && (isJust thisR) ||
                        (isJust thisL)  && (isJust foundR) then
                     keepJust (Just depth) lowerAnc
                   else
                     lowerAnc
      in ((keepJust thisL foundL), (keepJust thisR foundR), ancestor)
        


distBetween :: OrbitMap -> String -> String -> Maybe Int
distBetween m from to =
  let
    fromNode = (nameToId m) HashMap.! from
    toNode = (nameToId m) HashMap.! to
    comNode = (nameToId m)  HashMap.! "COM"
    (tree:[]) = Graph.dfs (orbitMap m) [comNode]
  in travelDistance tree fromNode toNode

someFunc :: IO ()
someFunc = putStrLn "someFunc"
