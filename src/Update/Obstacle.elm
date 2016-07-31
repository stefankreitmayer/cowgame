module Update.Obstacle exposing (stepObstacles)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Obstacle exposing (..)


stepObstacles : Time -> Scene -> Scene
stepObstacles delta ({obstacles} as scene) =
  let
      obstacles' = List.map (stepObstacle delta) obstacles
                 |> List.filter (\obstacle -> not obstacle.expired)
  in
      { scene | obstacles = obstacles' }


stepObstacle : Time -> Obstacle -> Obstacle
stepObstacle delta ({positionX,velocityX} as obstacle) =
  let
      positionX' = positionX + delta*velocityX
      expired = positionX > 1
      fadeIn = 30.0
      opacity = if positionX < 1.0/fadeIn then
                    positionX * fadeIn
                else if positionX < (1.0-1.0/fadeIn) then
                    1.0
                else
                    (1-positionX) * fadeIn
  in
      { obstacle | positionX = positionX'
                 , expired = expired
                 , opacity = opacity }
