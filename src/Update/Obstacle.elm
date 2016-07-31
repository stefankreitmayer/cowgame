module Update.Obstacle exposing (stepObstacles)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Obstacle exposing (..)
import Model.Scene.Particle exposing (..)


stepObstacles : Time -> Scene -> Scene
stepObstacles delta ({obstacles,player} as scene) =
  let
      obstacles' = obstacles |> List.map (stepObstacle delta player)
      newParticles =
        List.filterMap (explodeIfDead scene.absoluteTime) obstacles'
        |> List.concat
      aliveObstacles = obstacles' |> List.filter (\ob -> ob.status==Alive)
  in
      { scene | obstacles = aliveObstacles
              , particles = scene.particles ++ newParticles }


stepObstacle : Time -> Player -> Obstacle -> Obstacle
stepObstacle delta player ({positionX,velocityX} as obstacle) =
  let
      positionX' = positionX + delta*velocityX
      crushed = isUnderUdder positionX && player.positionY==0
      status = if positionX > 1 then
                   Expired
               else if crushed then
                   Dead
               else
                   obstacle.status
      fadeIn = 30.0
      opacity = if status==Dead then
                    0
                else if positionX < 1.0/fadeIn then
                    positionX * fadeIn
                else if positionX < (1.0-1.0/fadeIn) then
                    1.0
                else
                    (1-positionX) * fadeIn
  in
      { obstacle | positionX = positionX'
                 , status = status
                 , opacity = opacity }


isUnderUdder : Float -> Bool
isUnderUdder posX =
  posX > 0.78 && posX < 0.9


explodeIfDead : Time -> Obstacle -> Maybe (List Particle)
explodeIfDead absoluteTime obstacle =
  case obstacle.status of
    Dead ->
      Just (explode absoluteTime obstacle)

    _ ->
      Nothing
