module Model.Scene.Particle exposing (Particle,explode)

import Time exposing (Time)

import Model.Scene.Common exposing (..)
import Model.Scene.Obstacle exposing (..)

type alias Particle =
  { createdAt : Time
  , lifetime : Time
  , position : Vector
  , velocity : Vector }

type alias Vector =
  { x : Float
  , y : Float }


explode : Time -> Obstacle -> List Particle
explode absoluteTime obstacle =
  List.map (createParticle absoluteTime obstacle) [0..30]


createParticle : Time -> Obstacle -> Int -> Particle
createParticle absoluteTime {positionX,velocityX} index =
  let
      angle = (toFloat index)/20.0 + 4.0
      speed = 0.001
      position = { x = positionX, y = 0 }
      velocity =
        { x = (cos angle) * speed
        , y = (sin angle) * speed - 0.01}
  in
     Particle absoluteTime particleLifetime position velocity
