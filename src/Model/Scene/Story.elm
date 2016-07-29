module Model.Scene.Story exposing (..)

import Time exposing (Time)

type alias Story =
  List StoryEvent

type alias StoryEvent =
  { startTime : Time
  , text : String }


initialStory : Story
initialStory =
  initialStoryWithRelativeEntryDelays |> convertToAbsoluteTime 0


convertToAbsoluteTime : Time -> List StoryEvent -> List StoryEvent
convertToAbsoluteTime accumulatedTime events =
  case events of
    [] ->
      []

    hd::tl ->
      let
          acc = accumulatedTime + hd.startTime
          newHead = { hd | startTime = acc }
      in
          newHead :: (convertToAbsoluteTime acc tl)


initialStoryWithRelativeEntryDelays : List StoryEvent
initialStoryWithRelativeEntryDelays =
  [ StoryEvent 2000 "click to jump"
  , StoryEvent 3000 "oh yeah"

  , StoryEvent 3000 "hop over the fences"
  , StoryEvent 6000 "or crush them|if you prefer"
  , StoryEvent 2000 "really up to you"

  , StoryEvent 8000 "careful with|those pineapples"
  , StoryEvent 2000 "they are harmless"

  , StoryEvent 8000 "a huge wave of|zombies is approaching"
  , StoryEvent 4000 "brains|brains"

  , StoryEvent 8000 "you just earned|16777216 points"
  , StoryEvent 2000 "not really but|imagine you did"
  , StoryEvent 4000 "would you donate|all of them?"

  , StoryEvent 8000 "these balloons|are for you"

  , StoryEvent 8000 "attention!"
  , StoryEvent 4000 "jupiter behind you"
  , StoryEvent 5000 "that was close"

  , StoryEvent 5000 "more pineapples?"

  , StoryEvent 8000 "hey look"
  , StoryEvent 2000 "glad you enjoyed this"
  , StoryEvent 2000 "now please move|and get some air"
  ]
