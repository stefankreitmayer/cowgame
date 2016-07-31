module View.Particle exposing (renderParticles)

import Svg exposing (Svg,Attribute)
import Svg.Attributes

import Model.Scene.Particle exposing (..)
import View.Coordinates exposing (..)

import Msg exposing (..)


renderParticles : (Int,Int) -> List Particle -> Svg Msg
renderParticles windowSize particles =
  List.map (renderParticle windowSize) particles
  |> Svg.g []


renderParticle : (Int,Int) -> Particle -> Svg Msg
renderParticle windowSize particle =
  let
      zoom = globalZoom windowSize
      x = (leftBorder windowSize) + zoom*particle.position.x
      y = (groundPositionY windowSize |> toFloat) + zoom*particle.position.y
      r = zoom*0.01 |> round
  in
      Svg.circle
        [ Svg.Attributes.cx (toString x)
        , Svg.Attributes.cy (toString y)
        , Svg.Attributes.r (toString r) ]
        []
