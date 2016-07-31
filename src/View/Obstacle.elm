module View.Obstacle exposing (renderObstacles)

import Svg exposing (Svg,Attribute)
import Svg.Attributes

import Model.Scene.Obstacle exposing (..)
import View.Coordinates exposing (..)

import Msg exposing (..)


renderObstacles : (Int,Int) -> List Obstacle -> Svg Msg
renderObstacles windowSize obstacles =
  List.map (renderObstacle windowSize) obstacles
  |> Svg.g []


renderObstacle : (Int,Int) -> Obstacle -> Svg Msg
renderObstacle windowSize obstacle =
  let
      zoom = globalZoom windowSize
      posX = (leftBorder windowSize) + zoom*obstacle.positionX |> floor
      groundPosY = groundPositionY windowSize
      child =
        case obstacle.face of
          Fence ->
            renderFence posX groundPosY zoom

          Pineapple ->
            renderPineapple posX groundPosY zoom

          Zombie ->
            renderZombie posX groundPosY zoom
  in
      Svg.g
        [ Svg.Attributes.opacity (toString obstacle.opacity) ]
        [ child ]


renderFence : Int -> Int -> Float -> Svg Msg
renderFence posX groundPosY zoom =
  let
      sizeX = zoom*0.006 |> floor
      sizeY = zoom*0.03 |> floor
  in
      Svg.rect
        [ Svg.Attributes.x (toString posX)
        , Svg.Attributes.y (toString (groundPosY-sizeY))
        , Svg.Attributes.width (toString sizeX)
        , Svg.Attributes.height (toString sizeY) ]
        []


renderPineapple : Int -> Int -> Float -> Svg Msg
renderPineapple posX groundPosY zoom =
  let
      r = zoom*0.01 |> floor
  in
      Svg.circle
        [ Svg.Attributes.cx (toString posX)
        , Svg.Attributes.cy (toString (groundPosY-r))
        , Svg.Attributes.r (toString r) ]
        []


renderZombie : Int -> Int -> Float -> Svg Msg
renderZombie posX groundPosY zoom =
  let
      sizeX = zoom*0.006 |> floor
      sizeY = zoom*0.03 |> floor
      body = Svg.rect
               [ Svg.Attributes.x (toString posX)
               , Svg.Attributes.y (toString (groundPosY-sizeY))
               , Svg.Attributes.width (toString sizeX)
               , Svg.Attributes.height (toString sizeY) ]
               []
      arms = Svg.rect
               [ Svg.Attributes.x (toString posX)
               , Svg.Attributes.y (toString (groundPosY-sizeY*3//4))
               , Svg.Attributes.width (toString (sizeX*3))
               , Svg.Attributes.height (toString sizeX) ]
               []
  in
      Svg.g [] [ body, arms ]
