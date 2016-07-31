module Model.Scene.Obstacle exposing (..)

type alias Obstacle =
  { positionX : Float
  , velocityX : Float
  , face : ObstacleFace
  , expired : Bool
  , opacity : Float }

type ObstacleFace
  = Fence
  | Pineapple
  | Zombie
