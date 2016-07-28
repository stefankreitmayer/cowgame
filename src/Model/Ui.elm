module Model.Ui exposing (..)

import Set exposing (Set)
import Char

import Model.Scene exposing (..)


type alias Ui =
  { windowSize : (Int, Int) }


initialUi : Ui
initialUi =
  { windowSize = (500,500) }
