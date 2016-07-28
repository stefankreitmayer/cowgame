module Model exposing (..)

import Time exposing (Time)
import Set

import Model.Ui exposing (..)
import Model.Scene exposing (..)


type alias Model =
  { ui : Ui
  , scene : Scene }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene }
