module Model.Scene.Story exposing (..)

import Time exposing (Time)

type alias Story =
  List TimedOccurrence

type alias TimedOccurrence =
  { startTime : Time
  , occurrence : Occurrence }

type Occurrence
  = AnnouncementOccurrence String


initialStory : Story
initialStory =
  initialStoryWithRelativeEntryDelays |> convertToAbsoluteTime 0


convertToAbsoluteTime : Time -> List TimedOccurrence -> List TimedOccurrence
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


initialStoryWithRelativeEntryDelays : List TimedOccurrence
initialStoryWithRelativeEntryDelays =
  [ announce 2000 "click to jump"
  , announce 3000 "oh yeah"

  , announce 3000 "hop over the fences"
  , announce 6000 "or crush them|if you prefer"
  , announce 2000 "really up to you"

  , announce 8000 "careful with|those pineapples"
  , announce 2000 "they are harmless"

  , announce 8000 "a huge wave of|zombies is approaching"
  , announce 4000 "brains|brains"

  , announce 8000 "you just earned|16777216 points"
  , announce 2000 "not really|there are no zombies"
  , announce 2000 "but imagine|you won those points"
  , announce 4000 "would you wish|to donate them?"

  , announce 8000 "these balloons|are for you"

  , announce 8000 "attention!"
  , announce 4000 "jupiter behind you"
  , announce 5000 "that was close"

  , announce 5000 "more pineapples?"

  , announce 8000 "hey look"
  , announce 2000 "glad you enjoyed this"
  , announce 2000 "now please move|and get some air"
  ]


announce : Time -> String -> TimedOccurrence
announce startTime message =
  TimedOccurrence startTime (AnnouncementOccurrence message)
