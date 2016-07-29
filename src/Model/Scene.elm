module Model.Scene exposing (..)

import Char exposing (toCode)
import Time exposing (Time)

import Model.Geometry exposing (..)
import Model.Scene.Story exposing (..)


type alias Scene =
  { absoluteTime : Time
  , announcement : Maybe Announcement
  , textSpring : Elastic
  , story : Story
  , player : Player }

type alias Announcement =
  { createdAt : Time
  , text : String }

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
  , announcement = Nothing
  , textSpring = Elastic 0 0
  , story = initialStory
  , player = Player 0 0 0 }


playerPosX : Float
playerPosX = 0.5


groundPosX : Float
groundPosX = 0.5
