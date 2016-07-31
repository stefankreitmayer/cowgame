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
  | GameoverOccurrence


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
  , addObstacle 1300 Fence fenceSpeed
  , addObstacle 800 Fence fenceSpeed
  , announce 2900 "ain't no fence"
  , announce 2000 "gonna hold u back"
  , addObstacle 1100 Fence fenceSpeed
  , addObstacle 1500 Fence fenceSpeed
  , addObstacle 900 Fence fenceSpeed
  , addObstacle 100 Fence fenceSpeed
  , addObstacle 900 Fence fenceSpeed
  , announce 2900 "you are simply"
  , addObstacle 700 Fence fenceSpeed
  , announce 2400 "too awesome"

  , announce 4000 "ain't no mess"
  , addObstacle 300 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , addObstacle 900 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 400 Pineapple fenceSpeed
  , addObstacle 1000 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , announce 2500 "ever mess|with your head"
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 500 Pineapple fenceSpeed
  , addObstacle 800 Pineapple fenceSpeed
  , addObstacle 300 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , addObstacle 900 Pineapple fenceSpeed
  , addObstacle 200 Pineapple fenceSpeed
  , addObstacle 400 Pineapple fenceSpeed
  , addObstacle 450 Pineapple fenceSpeed
  , announce 2900 "coz you're a pro"

  , announce 7000 "a huge wave|of zombies"
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
  , announce 0 "so what?"
  , addObstacle 170 Zombie (zombieSpeed*1.13)
  , addObstacle 210 Zombie (zombieSpeed*1.12)

  , announce 3500 "zombies schmombies"

  , announce 7000 "alrighty|holy cow"
  , announce 3000 "that was lovely|thanks"
  , announce 3000 "see you soon"
  , gameover 1000
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


gameover : Time -> TimedOccurrence
gameover startTime =
  TimedOccurrence startTime GameoverOccurrence


fenceSpeed : Float
fenceSpeed = 0.0004


zombieSpeed : Float
zombieSpeed = 0.0001
