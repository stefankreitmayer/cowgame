module Update.Story exposing (updateStory)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Obstacle exposing (..)
import Model.Scene.Story exposing (..)


updateStory : Scene -> Scene
updateStory ({absoluteTime,announcement,obstacles,story} as scene) =
  case story of
    {startTime,occurrence} :: restOfStory ->
      if absoluteTime > startTime then
          { scene | story = restOfStory }
          |> handleOccurrence startTime occurrence
          |> updateStory
      else
          scene

    _ ->
        scene


handleOccurrence : Time -> Occurrence -> Scene -> Scene
handleOccurrence time occurrence scene =
  case occurrence of
    AnnouncementOccurrence text ->
      { scene | announcement = Announcement time text True 0 }

    ObstacleOccurrence obstacle ->
      { scene | obstacles = obstacle :: scene.obstacles }
