module Update.Player exposing (stepPlayer,jump)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Common exposing (..)


stepPlayer : Time -> Scene -> Scene
stepPlayer delta ({player} as scene) =
  let
      player' =
        if isJumping player then
            fly delta player
        else
            player
  in
      { scene | player = player' }


fly : Time -> Player -> Player
fly delta ({velocityY,positionY} as player) =
  let
      velocityY' = velocityY + gravityConstant * delta
      positionY' = positionY + velocityY' |> min 0
      (positionY'', velocityY'') = if positionY' < 0 then
                                       (positionY',velocityY')
                                   else
                                       (0,0)
  in
      { player | positionY = positionY''
               , velocityY = velocityY'' }


jump : Player -> Player
jump player =
  if isJumping player then
      player
  else
      { player | velocityY = -0.02 }


isJumping : Player -> Bool
isJumping {positionY,velocityY} =
  positionY < 0 || velocityY < 0
