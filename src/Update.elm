module Update exposing (..)

import Set exposing (Set)
import Char
import Time exposing (Time)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Scene.Story exposing (..)
import Model.Scene.Obstacle exposing (..)
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
          scene' = { scene | absoluteTime = scene.absoluteTime + delta }
          (obstacles,announcement,story) = updateStory scene'
          player = scene'.player
          player' =
            if isJumping player then
                fly delta player
            else
                player
          announcement' = announcement |> updateAnnouncement scene'.absoluteTime
          textSpring' = scene'.textSpring |> updateTextSpring player'
          obstacles' = updateObstacles delta obstacles
          scene'' = { scene' | player = player'
                             , announcement = announcement'
                             , textSpring = textSpring'
                             , story = story
                             , obstacles = obstacles' }
      in
          ({ model | scene = scene'' }, Cmd.none)

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


updateStory : Scene -> (List Obstacle,Announcement,Story)
updateStory ({absoluteTime,announcement,obstacles,story} as scene) =
  case story of
    {startTime,occurrence} :: restOfStory ->
      if absoluteTime > startTime then
          { scene | story = restOfStory }
          |> handleOccurrence startTime occurrence
          |> updateStory
      else
          (obstacles,announcement,story)

    _ ->
      (obstacles,announcement,story)


handleOccurrence : Time -> Occurrence -> Scene -> Scene
handleOccurrence time occurrence scene =
  case occurrence of
    AnnouncementOccurrence text ->
      { scene | announcement = Announcement time text True 0 }

    ObstacleOccurrence ->
      { scene | obstacles = (Obstacle 0 0.0005) :: scene.obstacles }


updateAnnouncement : Time -> Announcement -> Announcement
updateAnnouncement absoluteTime announcement =
  let
      isExpired = absoluteTime > announcement.createdAt + 1800
      visible = not isExpired
      opacity = 0.4*announcement.opacity + 0.6 * (if visible then 1 else 0)
  in
      { announcement | visible = visible
                     , opacity = opacity }


updateObstacles : Time -> List Obstacle -> List Obstacle
updateObstacles delta obstacles =
  List.map (updateObstacle delta) obstacles


updateObstacle : Time -> Obstacle -> Obstacle
updateObstacle delta ({positionX,velocityX} as obstacle) =
  { obstacle | positionX = positionX + delta*velocityX }
