module View.Coordinates exposing (..)

import Svg exposing (Svg,Attribute)
import Svg.Attributes exposing (textAnchor)

import Model.Scene.Obstacle exposing (..)


playerImageWidth : Float
playerImageWidth =
  646.0


playerAspectRatio : Float
playerAspectRatio =
  646.0/328.0


groundAspectRatio : Float
groundAspectRatio =
  646.0/5.0


groundPositionY : (Int,Int) -> Int
groundPositionY ((_,h) as windowSize) =
  h//2 + (playerSizeY windowSize)//2


playerSizeY : (Int,Int) -> Int
playerSizeY windowSize =
  (globalZoom windowSize) / playerAspectRatio |> floor


globalZoom : (Int,Int) -> Float
globalZoom (w,h) =
  (w |> toFloat) * 0.9 |> min ((h |> toFloat) * 1.6) |> min playerImageWidth


leftBorder : (Int,Int) -> Float
leftBorder ((w,h) as windowSize) =
  (toFloat w)/2 - (globalZoom windowSize)/2

