module Msg exposing (..)

import Time exposing (Time)

import Model exposing (..)
import Model.Ui exposing (..)
import Window
import Task
import AnimationFrame


type Msg
  = ResizeWindow (Int,Int)
  | Tick Time
  | Jump
  | NoOp


subscriptions : Model -> Sub Msg
subscriptions {ui} =
  let
      window = Window.resizes (\{width,height} -> ResizeWindow (width,height))
      animation = AnimationFrame.diffs Tick
  in
      Sub.batch [ window, animation ]

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
