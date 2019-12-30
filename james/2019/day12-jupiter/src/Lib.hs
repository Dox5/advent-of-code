module Lib where

import Parse

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State

import qualified Data.List as List


data Planet = Planet {
    position :: Vector,
    velocity :: Vector
  }
  deriving (Show)

type Planets = [Planet]

transpose3_2 :: ((a, a), (a, a), (a, a)) -> ((a, a, a), (a, a, a))
transpose3_2 ((x1, x2), (y1, y2), (z1, z2)) = ((x1, y1, z1), (x2, y2, z2))

calcChange :: Int -> Int -> (Int, Int)
calcChange c1 c2 =
  if c1 > c2 then
    (-1, 1)
  else if c1 < c2 then
    (1, -1)
  else
    (0, 0)
    

calcVelChanges :: Planet -> Planet -> (Vector, Vector)
calcVelChanges planet1 planet2 =
  let
    (x1, y1, z1) = position planet1
    (x2, y2, z2) = position planet2
  in transpose3_2 ((calcChange x1 x2), (calcChange y1 y2), (calcChange z1 z2))

applyVeloctiyChange :: Planet -> Vector -> Planet
applyVeloctiyChange p (dx, dy, dz) =
  let
    (x, y, z) = velocity p
  in p{velocity = (x + dx, y + dy, z + dz)}

applyVelocty :: Planet -> Vector -> Planet
applyVelocty p (dx, dy, dz) =
  let
    (x, y, z) = position p
  in p{position = (x + dx, y + dy, z + dz)}

sumVec :: Vector -> Int
sumVec (x, y, z) = (abs x) + (abs y) + (abs z)

kineticEnergy :: Planet -> Int
kineticEnergy = sumVec . velocity

potentialEnergy :: Planet -> Int
potentialEnergy = sumVec . position

planetEnergy :: Planet -> Int
planetEnergy p =
  let
    kinetic = kineticEnergy p
    potential = potentialEnergy p
  in kinetic * potential

updateVelocities :: State Planets ()
updateVelocities = State.modify walk
  where
    walk :: Planets -> Planets
    walk (x:xs) =
      let
        (done, planets) = singlePass x xs []
        xs' = State.execState updateVelocities planets
      in seq done $ done:xs'
        
    walk last = last

    singlePass :: Planet -> Planets -> Planets -> (Planet, Planets)
    singlePass p [] updated = (p, updated)
    singlePass p (x:xs) updated =
      let
        (dp, dx) = calcVelChanges p x
        p' = applyVeloctiyChange p dp
        x' = applyVeloctiyChange x dx
      in singlePass p' xs (x':updated)

applyVelocties :: State Planets ()
applyVelocties = State.modify (\planets -> case planets of
                                [] -> []
                                (p:ps) ->
                                  let
                                    v = velocity p
                                    p' = applyVelocty p v
                                    ps' = State.execState applyVelocties ps
                                  in p':ps')


systemEnergy :: State Planets Int
systemEnergy = State.gets (\ planets ->
                            List.foldl' (\a p -> a + (planetEnergy p)) 0 planets)


step :: State Planets ()
step = do
  updateVelocities
  applyVelocties
