module Update exposing (..)

import Set exposing (Set)
import Char
import Time exposing (Time)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Scene.Story exposing (..)
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
          textSpring' = scene.textSpring |> updateTextSpring player'
          (announcement',story') = updateAnnouncement scene
          opacity' = 0.4*announcement'.opacity + 0.6 * (if announcement'.visible then 1 else 0)
          announcement'' = { announcement' | opacity = opacity' }
          scene' = { scene | absoluteTime = scene.absoluteTime + delta
                           , player = player'
                           , announcement = announcement''
                           , textSpring = textSpring'
                           , story = story' }
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


updateTextSpring : Player -> Elastic -> Elastic
updateTextSpring player ({pos,vel} as textSpring) =
  let
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
  in
      { textSpring | pos = pos''
                   , vel = vel''}


updateAnnouncement : Scene -> (Announcement,Story)
updateAnnouncement {absoluteTime,announcement,story} =
  case nextStoryEvent story of
    Nothing ->
      (announcement,story)

    Just nextEvent ->
      let
          isDue = absoluteTime > nextEvent.startTime
      in
          if isDue then
              (Announcement absoluteTime nextEvent.text True 0, List.drop 1 story)
          else if isExpired absoluteTime announcement then
              ({ announcement | visible = False },story)
          else
              (announcement,story)


isExpired : Time -> Announcement -> Bool
isExpired absoluteTime announcement =
  absoluteTime > announcement.createdAt + 1800


nextStoryEvent : Story -> Maybe StoryEvent
nextStoryEvent story =
  List.head story
