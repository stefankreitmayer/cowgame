module View exposing (view)

import Html exposing (Html,div)
import Html.Attributes exposing (class)
import Html.Events
import Svg exposing (Svg,Attribute)
import Svg.Attributes exposing (fill,fontFamily,textAnchor)
import Svg.Events
import Time exposing (Time)
import String

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Msg exposing (..)

import VirtualDom
import Json.Encode as Json


view : Model -> Html Msg
view {ui,scene} =
  renderPlayScreen ui.windowSize scene


renderPlayScreen : (Int,Int) -> Scene -> Html.Html Msg
renderPlayScreen (w,h) ({player} as scene) =
  let
      windowSize = (w,h)
  in
     div
       []
       [ renderPlayer windowSize player
       , renderSvg windowSize scene
       , renderTransparentJumpButton windowSize ]


renderAnnouncement : (Int,Int) -> Maybe Announcement -> Html Msg
renderAnnouncement windowSize announcement =
  case announcement of
    Nothing ->
      div [] []

    Just {text} ->
      textOnCow windowSize text


renderTransparentJumpButton : (Int,Int) -> Html.Html Msg
renderTransparentJumpButton (w,h) =
  div
    [ Html.Events.onClick Jump
    , class "jumpbutton" ]
    []


renderSvg : (Int,Int) -> Scene -> Html Msg
renderSvg windowSize scene =
  Svg.svg
    (svgAttributes windowSize)
    [ renderAnnouncement windowSize scene.announcement ]

svgAttributes : (Int, Int) -> List (Attribute Msg)
svgAttributes (w, h) =
  [ Svg.Attributes.width (toString w)
  , Svg.Attributes.height (toString h)
  , Svg.Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
  , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
  , Svg.Attributes.version "1.1"
  , Svg.Attributes.style "position: fixed;"
  ]


renderPlayer : (Int,Int) -> Player -> Html Msg
renderPlayer (w,h) {positionY} =
  let
      wf = w |> toFloat
      zoom = wf * 0.9 |> min ((h |> toFloat) * 1.6) |> min playerImageWidth
      sx = zoom |> floor
      playerSY = zoom / playerAspectRatio |> floor
      x = w//2 - sx//2
      groundSY = zoom / groundAspectRatio |> floor
      groundY = h//2 + playerSY//2
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


normalTextColor : String
normalTextColor = "rgba(0,0,0,.9)"


normalFontFamily : String
normalFontFamily =
  "Courier New, Courier, Monaco, monospace"


normalFontSize : Int -> Int -> Int
normalFontSize w h =
  (min w h) // 20 |> min 24


normalLineHeight : Int -> Int -> Int
normalLineHeight w h =
  (toFloat (normalFontSize w h)) * 1.38 |> floor


textOnCow : (Int,Int) -> String -> Svg Msg
textOnCow (w,h) text =
  renderTextLine (w*5//9) (h*1//2) (normalFontSize w h) "middle" text []


renderTextLine : Int -> Int -> Int -> String -> String -> List (Svg.Attribute Msg) -> Svg Msg
renderTextLine xPos yPos fontSize anchor content extraAttrs =
  let
      attributes = [ Svg.Attributes.x <| toString xPos
                   , Svg.Attributes.y <| toString yPos
                   , textAnchor anchor
                   , fontFamily normalFontFamily
                   , Svg.Attributes.fontSize (toString fontSize)
                   , fill normalTextColor
                   ]
                   |> List.append extraAttrs
  in
      Svg.text' attributes [ Svg.text content ]


playerImageWidth : Float
playerImageWidth =
  646.0


playerAspectRatio : Float
playerAspectRatio =
  646.0/328.0


groundAspectRatio : Float
groundAspectRatio =
  646.0/5.0
