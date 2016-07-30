module Update exposing (..)

import Time exposing (Time)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)

import Update.Story exposing (..)
import Update.Player exposing (..)
import Update.Obstacle exposing (..)

import Msg exposing (..)

import Debug exposing (log)


update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          scene' = { scene | absoluteTime = scene.absoluteTime + delta }
                 |> updateStory
                 |> stepPlayer delta
                 |> updateAnnouncement
                 |> stepTextSpring
                 |> stepObstacles delta
      in
          ({ model | scene = scene' }, Cmd.none)

    Jump ->
      let
          player' = scene.player |> jump
          scene' = { scene | player = player' }
          model' = { model | scene = scene' }
      in
          (model', Cmd.none)

    NoOp ->
      (model, Cmd.none)


updateAnnouncement : Scene -> Scene
updateAnnouncement ({announcement,absoluteTime} as scene) =
  let
      isExpired = absoluteTime > announcement.createdAt + 1800
      visible = not isExpired
      opacity = 0.4*announcement.opacity + 0.6 * (if visible then 1 else 0)
      announcement' = { announcement | visible = visible
                                     , opacity = opacity }
  in
      { scene | announcement = announcement' }


stepTextSpring : Scene -> Scene
stepTextSpring ({textSpring,player} as scene) =
  let
      pos = textSpring.pos
      vel = textSpring.vel
      inertia = 0.8
      attraction = 0.05
      vel' = if player.positionY < 0 then
                 player.velocityY*1.05 - 0.001
             else
                 vel * inertia + attraction * -pos
      pos' = pos + vel'
      (pos'',vel'') =
        if (abs pos')+(abs vel') < 0.0002 then
            (0,0)
        else
            (pos',vel')
      textSpring' = { textSpring | pos = pos''
                                 , vel = vel'' }
  in
      { scene | textSpring = textSpring' }
