module Model.Scene.Story exposing (..)

import Time exposing (Time)

import Model.Scene.Obstacle exposing (..)


type alias Story =
  List TimedOccurrence

type alias TimedOccurrence =
  { startTime : Time
  , occurrence : Occurrence }

type Occurrence
  = AnnouncementOccurrence String
  | ObstacleOccurrence Obstacle


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

  , announce 1300 "you can hop|over fences"
  , addObstacle 1600 Fence fenceSpeed
  , addObstacle 2500 Fence fenceSpeed
  , addObstacle 900 Fence fenceSpeed
  , addObstacle 1100 Fence fenceSpeed
  , addObstacle 100 Fence fenceSpeed
  , announce 1500 "if you want"
  , addObstacle 700 Fence fenceSpeed
  , addObstacle 1500 Fence fenceSpeed
  , addObstacle 600 Fence fenceSpeed
  , addObstacle 800 Fence fenceSpeed

  , announce 4000 "these are|not pineapples"
  , addObstacle 1800 Pineapple fenceSpeed
  , addObstacle 900 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 400 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 500 Pineapple fenceSpeed
  , addObstacle 800 Pineapple fenceSpeed
  , announce 1000 "i don't know|what they are"
  , addObstacle 300 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , announce 500 "but they seem harmless"
  , addObstacle 900 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 400 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed

  , announce 8000 "a huge wave of|zombies is approaching"
  , addObstacle 210 Zombie (zombieSpeed*1.19)
  , addObstacle 230 Zombie (zombieSpeed*1.19)
  , addObstacle 340 Zombie (zombieSpeed*1.18)
  , addObstacle 170 Zombie (zombieSpeed*1.13)
  , addObstacle 210 Zombie (zombieSpeed*1.12)
  , addObstacle 110 Zombie (zombieSpeed*1.02)
  , addObstacle 130 Zombie (zombieSpeed*1.13)
  , addObstacle 380 Zombie (zombieSpeed*1.12)
  , addObstacle 150 Zombie (zombieSpeed*1.12)
  , addObstacle 330 Zombie (zombieSpeed*1.16)
  , addObstacle 190 Zombie (zombieSpeed*1.15)
  , addObstacle 140 Zombie (zombieSpeed*1.14)
  , addObstacle 250 Zombie (zombieSpeed*1.13)
  , addObstacle 110 Zombie (zombieSpeed*1.12)
  , addObstacle 250 Zombie (zombieSpeed*1.11)
  , addObstacle 510 Zombie (zombieSpeed*1.10)
  , announce 1000 "brains|brains"

  , announce 8000 "you just earned|16777216 points"
  , announce 4000 "would you like|to donate them?"

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


addObstacle : Time -> ObstacleFace -> Float -> TimedOccurrence
addObstacle startTime face speed =
  let
      obstacle = Obstacle 0 speed face False 0
  in
      TimedOccurrence startTime (ObstacleOccurrence obstacle)


fenceSpeed : Float
fenceSpeed = 0.0004


zombieSpeed : Float
zombieSpeed = 0.0001
