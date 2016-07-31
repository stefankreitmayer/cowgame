module Model.Scene.Particle exposing (Particle,explode)

import Time exposing (Time)

import Model.Scene.Common exposing (..)
import Model.Scene.Obstacle exposing (..)

type alias Particle =
  { createdAt : Time
  , lifetime : Time
  , position : Vector
  , velocity : Vector
  , angle : Float }

type alias Vector =
  { x : Float
  , y : Float }


explode : Time -> Obstacle -> List Particle
explode absoluteTime obstacle =
  List.map (createParticle absoluteTime obstacle) [ 0..7 ]


createParticle : Time -> Obstacle -> Int -> Particle
createParticle absoluteTime {positionX,velocityX} index =
  let
      angle = (toFloat index)/5.0 + 4.0
      speed = 0.0002 + ((index%3) |> toFloat)*0.0006
      udderDistance = positionX - (udderStartX*3+udderEndX)/4
      tossHorizontal = 0.02 * udderDistance - 0.0005
      position = { x = positionX, y = (toFloat index) * -0.005 }
      velocity =
        { x = (cos angle) * speed + tossHorizontal
        , y = (sin angle) * speed }
  in
     Particle absoluteTime particleLifetime position velocity (3.141/2)
