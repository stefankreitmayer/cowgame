module Model.Scene exposing (..)

import Char exposing (toCode)
import Time exposing (Time)

import Model.Scene.Story exposing (..)
import Model.Scene.Obstacle exposing (..)
import Model.Scene.Particle exposing (..)


type alias Scene =
  { absoluteTime : Time
  , announcement : Announcement
  , textSpring : Elastic
  , story : Story
  , player : Player
  , obstacles : List Obstacle
  , particles : List Particle }

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
  , obstacles = []
  , particles = [] }
