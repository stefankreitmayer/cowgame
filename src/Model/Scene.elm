module Model.Scene exposing (..)

import Char exposing (toCode)
import Time exposing (Time)

import Model.Geometry exposing (..)


type alias Scene =
  { time : Time
  , startTime : Maybe Time
  , player : Player }

type alias Player =
  { score : Int
  , positionY : Float
  , velocityY : Float }


initialScene : Scene
initialScene =
  { time = 0
  , startTime = Nothing
  , player = Player 0 0 0 }

playerPosX : Float
playerPosX = 0.5

groundPosX : Float
groundPosX = 0.5


hasGameStarted : Scene -> Bool
hasGameStarted {startTime} =
  case startTime of
    Nothing ->
      False

    Just _ ->
      True
