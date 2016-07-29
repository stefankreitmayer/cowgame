module Model.Scene exposing (..)

import Char exposing (toCode)
import Time exposing (Time)

import Model.Geometry exposing (..)
import Model.Scene.Story exposing (..)
import Model.Scene.Obstacle exposing (..)


type alias Scene =
  { absoluteTime : Time
  , announcement : Announcement
  , textSpring : Elastic
  , story : Story
  , player : Player
  , obstacle : Obstacle }

type alias Announcement =
  { createdAt : Time
  , text : String
  , visible : Bool
  , opacity : Float }

type alias Elastic =
  { pos : Float
  , vel : Float }

type alias Player =
  { score : Int
  , positionY : Float
  , velocityY : Float }


initialScene : Scene
initialScene =
  { absoluteTime = 0
  , announcement = Announcement 0 "" False 0
  , textSpring = Elastic 0 0
  , story = initialStory
  , player = Player 0 0 0
  , obstacle = Obstacle 0 0.0002 }


playerPosX : Float
playerPosX = 0.5


groundPosX : Float
groundPosX = 0.5
