module Model.Scene.Obstacle exposing (..)

type alias Obstacle =
  { positionX : Float
  , velocityX : Float
  , face : ObstacleFace
  , status : ObstacleStatus
  , opacity : Float }

type ObstacleFace
  = Fence
  | Pineapple
  | Zombie

type ObstacleStatus
  = Alive
  | Dead
  | Expired
