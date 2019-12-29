module Lib where

import Stack (Stack)
import qualified Stack as Stack

import Data.Complex (Complex((:+)))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State

data Colour = Black | White
  deriving (Show, Eq)

type Vector = Complex Float
type Point = Complex Int
type Grid = HashMap Point (Stack Colour)

data Direction = Left | Right
  deriving (Show, Eq)

data Robot = Robot {
    bounds :: (Point, Point),
    bearing :: Vector,
    position :: Point,
    grid :: Grid
}
  deriving (Show)

toPoint :: RealFloat a => Complex a -> Point
toPoint (i :+ j) = (round(i) :+ round (j)) 

toVector :: Complex Int -> Complex Float
toVector (i :+ j) = (fromIntegral i) :+ (fromIntegral j)

defaultColour :: Colour
defaultColour = Black

north :: Vector
north = 0.0 :+ 1.0

rowWise :: (Point, Point) -> [[Point]]
rowWise ((blx :+ bly), (trx :+ try)) = [[(x :+ y) | x <- [blx..trx]] | y <- [try,try-1..bly]]

bootRobot :: Robot
bootRobot = Robot ((0 :+ 0), (0 :+ 0)) north (0 :+ 0) HashMap.empty

bootRobotWithPainted :: Colour -> Robot
bootRobotWithPainted initialColour =
  let
    defaultBot = Robot ((0 :+ 0), (0 :+ 0)) north (0 :+ 0) HashMap.empty
  in State.execState (paint initialColour) defaultBot

detectColour :: State Robot Colour
detectColour = State.gets (\r ->
  let
    paintLayers = HashMap.lookupDefault Stack.empty (position r) (grid r)
  in case State.evalState Stack.peek paintLayers of
    (Just c) -> c
    Nothing -> defaultColour
  )

paint :: Colour -> State Robot ()
paint c = do
  r <- State.get
  let m' = HashMap.alter (\paints -> case paints of
                                     (Just p) -> Just $ State.execState (Stack.push c) p
                                     Nothing -> Just $ State.execState (Stack.push c) Stack.empty) (position r) (grid r)
  State.put r{grid=m'}


turn :: Direction -> State Robot ()
turn d = let
  rotate = case d of
        Lib.Left  -> (0 :+ 1)
        Lib.Right -> (0 :+ (-1))
  in State.modify (\r -> r{bearing = rotate * (bearing r)}) 

updateBounds :: (Point, Point) -> Point -> (Point, Point)
updateBounds ((blx :+ bly), (trx :+ try)) (x :+ y) =
  (((min blx x) :+ (min bly y)), ((max trx x) :+ (max try y)))

move :: State Robot ()
move = State.modify(\r ->
  let
    newPos = toPoint $ (toVector $ position r) + (bearing r)
    newBounds = updateBounds (bounds r) newPos
  in r{bounds = newBounds, position = newPos})

render :: State Robot String
render = do
  b <- State.gets bounds
  g <- State.gets grid
  return $ concatMap (\s -> s ++ "\n") . fmap (fmap $ renderPoint g) $ rowWise b
    where
      renderPoint :: Grid -> Point -> Char
      renderPoint g p =
        let
          maybe_stack = HashMap.lookup p g
          maybe_colour = case maybe_stack of
                          (Just s) -> State.evalState Stack.pop s
                          Nothing  -> Just defaultColour
        in case maybe_colour of
          (Just Black) -> ' '
          (Just White) -> '#'
          Nothing -> error "Should've handled default case before now"
