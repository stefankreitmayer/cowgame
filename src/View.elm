module View exposing (view)

import Html exposing (Html,div)
import Html.Attributes exposing (class)
import Html.Events
import Svg exposing (Svg,Attribute)
import Svg.Attributes exposing (textAnchor)
import Svg.Events
import Time exposing (Time)
import String

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Scene.Obstacle exposing (..)

import View.Coordinates exposing (..)
import View.Obstacle exposing (..)

import Msg exposing (..)

import VirtualDom
import Json.Encode as Json


view : Model -> Html Msg
view {ui,scene} =
  div
    []
    [ renderPlayer ui.windowSize scene.player
    , renderSvg ui.windowSize scene
    , renderTransparentJumpButton ui.windowSize ]


renderSvg : (Int,Int) -> Scene -> Html Msg
renderSvg windowSize {announcement,textSpring,player,obstacles} =
  Svg.svg
    (svgAttributes windowSize)
    [ renderAnnouncement windowSize announcement textSpring
    , renderObstacles windowSize obstacles ]


svgAttributes : (Int, Int) -> List (Attribute Msg)
svgAttributes (w, h) =
  [ Svg.Attributes.width (toString w)
  , Svg.Attributes.height (toString h)
  , Svg.Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
  , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
  , Svg.Attributes.version "1.1"
  , Svg.Attributes.style "position: fixed;"
  ]


renderAnnouncement : (Int,Int) -> Announcement -> Elastic -> Html Msg
renderAnnouncement windowSize {text,opacity} {pos} =
  let
      offsetY = pos * (globalZoom windowSize) |> floor
  in
      textOnCow windowSize offsetY opacity text


renderTransparentJumpButton : (Int,Int) -> Html.Html Msg
renderTransparentJumpButton (w,h) =
  div
    [ Html.Events.onClick Jump
    , class "jumpbutton" ]
    []


renderPlayer : (Int,Int) -> Player -> Html Msg
renderPlayer windowSize {positionY} =
  let
      zoom = globalZoom windowSize
      x = leftBorder windowSize |> floor
      sx = zoom |> floor
      playerSY = playerSizeY windowSize
      groundY = groundPositionY windowSize
      groundSY = zoom / groundAspectRatio |> floor
      playerY = groundY - playerSY + (zoom * positionY |> floor)
  in
      div
        []
        [ renderImageFixedPosition "images/cow.gif" x playerY sx playerSY
        , renderImageFixedPosition "images/ground.gif" x groundY sx groundSY ]


renderImageFixedPosition : String -> Int -> Int -> Int -> Int -> Html Msg
renderImageFixedPosition url x y width height =
  let
      px number = (toString number) ++ "px"
      style = Html.Attributes.style [ ("position", "fixed")
                                    , ("left", px x)
                                    , ("top", px y)
                                    , ("width", px width)
                                    , ("height", px height) ]
      srcAttr = Html.Attributes.src url
  in
      Html.img [ srcAttr, style ] []


normalFontSize : (Int,Int) -> Int
normalFontSize windowSize =
  (globalZoom windowSize) / 20.0 |> floor


textOnCow : (Int,Int) -> Int -> Float -> String -> Svg Msg
textOnCow (w,h) offsetY opacity text =
  let
      lines = String.split "|" text
      lineHeight = normalFontSize (w,h)
      posY index = h * (50 - (List.length lines)) // 100 + offsetY + index * lineHeight*5//4
      print index = renderTextLine (w//2+lineHeight*16//10) (posY index) lineHeight "middle"
      styleString = "opacity: "++(toString opacity)
  in
      List.indexedMap print lines
      |> Svg.g
        [ Svg.Attributes.style styleString ]


renderTextLine : Int -> Int -> Int -> String -> String -> Svg Msg
renderTextLine xPos yPos fontSize anchor line =
  let
      attributes = [ Svg.Attributes.x <| toString xPos
                   , Svg.Attributes.y <| toString yPos
                   , textAnchor anchor
                   , Svg.Attributes.fontSize (toString fontSize)
                   ]
  in
      Svg.text' attributes [ Svg.text line ]
