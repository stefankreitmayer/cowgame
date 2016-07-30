module Update.Player exposing (..)

import Time exposing (Time)

import Model.Scene exposing (..)


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


gravityConstant : Float
gravityConstant =
  0.00007


isJumping : Player -> Bool
isJumping {positionY,velocityY} =
  positionY < 0 || velocityY < 0
