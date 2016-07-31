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

  , addObstacle 1600 Fence fenceSpeed
  , addObstacle 800 Fence fenceSpeed
  , addObstacle 1300 Fence fenceSpeed
  , announce 2200 "ain't no fence"
  , announce 2000 "gonna ever|hold u back"
  , addObstacle 1100 Fence fenceSpeed
  , addObstacle 100 Fence fenceSpeed
  , addObstacle 700 Fence fenceSpeed
  , addObstacle 1500 Fence fenceSpeed
  , announce 500 "you are simply"
  , addObstacle 900 Fence fenceSpeed
  , addObstacle 800 Fence fenceSpeed
  , addObstacle 700 Fence fenceSpeed
  , announce 500 "too awesome"
  , addObstacle 1500 Fence fenceSpeed

  , announce 4000 "ain't no poop"
  , addObstacle 1000 Pineapple fenceSpeed
  , addObstacle 900 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 400 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , announce 2000 "ever gonna touch you"
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 500 Pineapple fenceSpeed
  , addObstacle 800 Pineapple fenceSpeed
  , addObstacle 300 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
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
  , addObstacle 170 Zombie (zombieSpeed*1.13)
  , addObstacle 210 Zombie (zombieSpeed*1.12)
  , announce 0 "ain't no problem"
  , announce 3500 "ain't no brains|down there"

  , announce 5000 "you just earned|16777216 points"
  , announce 2000 "not saying|you need those"
  , announce 8000 "anyway - congratulations"

  , announce 3000 "this was fun"
  , announce 3000 "thank you|holy cow"
  , announce 3000 "see you soon"
  ]


announce : Time -> String -> TimedOccurrence
announce startTime message =
  TimedOccurrence startTime (AnnouncementOccurrence message)


addObstacle : Time -> ObstacleFace -> Float -> TimedOccurrence
addObstacle startTime face speed =
  let
      obstacle = Obstacle 0 speed face Alive 0
  in
      TimedOccurrence startTime (ObstacleOccurrence obstacle)


fenceSpeed : Float
fenceSpeed = 0.0004


zombieSpeed : Float
zombieSpeed = 0.0001
