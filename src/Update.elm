module Update exposing (..)

import Set exposing (Set)
import Char
import Time exposing (Time)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Geometry exposing (..)
import Msg exposing (..)

import Debug exposing (log)


update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          player = scene.player
          player' = if isJumping player then
              fly delta player
          else
              player
          scene' = { scene | player = player' }
      in
          ({ model | scene = scene' }, Cmd.none)

    Jump ->
      let
          player = scene.player
          player' = if isJumping player then
                        player
                    else
                        { player | velocityY = -0.02 }
          scene' = { scene | player = player' }
          model' = { model | scene = scene' }
      in
          (model', Cmd.none)

    NoOp ->
      (model, Cmd.none)


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
