module Model.Scene.Story exposing (..)

import Time exposing (Time)

type alias Story =
  List StoryEvent

type alias StoryEvent =
  { startTime : Time
  , text : String }


initialStory : Story
initialStory =
  [ (StoryEvent 3000 "click to jump")
  , (StoryEvent 6000 "great job!") ]
