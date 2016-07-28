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
          (announcement',story') = updateAnnouncement scene
          scene' = { scene | absoluteTime = scene.absoluteTime + delta
                           , player = player'
                           , announcement = announcement'
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


updateAnnouncement : Scene -> (Maybe Announcement,Story)
updateAnnouncement {absoluteTime,announcement,story} =
  case nextStoryEvent story of
    Nothing ->
      (announcement,story)

    Just nextEvent ->
      let
          isDue = absoluteTime > nextEvent.startTime
      in
          if isDue then
              (Just (Announcement absoluteTime nextEvent.text), List.drop 1 story)
          else if isExpired absoluteTime announcement then
              (Nothing,story)
          else
              (announcement,story)


isExpired : Time -> Maybe Announcement -> Bool
isExpired absoluteTime announcement =
  case announcement of
    Nothing ->
      False

    Just announcement ->
      absoluteTime > announcement.createdAt + 1800


nextStoryEvent : Story -> Maybe StoryEvent
nextStoryEvent story =
  List.head story
