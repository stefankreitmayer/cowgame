module Model.Scene.Obstacle exposing (..)

type alias Obstacle =
  { positionX : Float
  , velocityX : Float
  , face : ObstacleFace }

type ObstacleFace
  = Fence
  | Pineapple
  | Zombie
