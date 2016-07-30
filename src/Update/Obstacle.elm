module Update.Obstacle exposing (stepObstacles)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Obstacle exposing (..)


stepObstacles : Time -> Scene -> Scene
stepObstacles delta ({obstacles} as scene) =
  let
      obstacles' = List.map (stepObstacle delta) obstacles
  in
      { scene | obstacles = obstacles' }


stepObstacle : Time -> Obstacle -> Obstacle
stepObstacle delta ({positionX,velocityX} as obstacle) =
  { obstacle | positionX = positionX + delta*velocityX }
