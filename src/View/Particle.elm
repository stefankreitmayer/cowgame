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
      length = zoom*0.01
      dx = (cos particle.angle) * length/2
      dy = (sin particle.angle) * length/2
      x = (leftBorder windowSize) + zoom*particle.position.x
      y = (groundPositionY windowSize |> toFloat) + zoom*particle.position.y
      strokeWidth = length*0.7 |> toString
  in
      Svg.line
        [ Svg.Attributes.x1 (toString (x+dx))
        , Svg.Attributes.y1 (toString (y+dy))
        , Svg.Attributes.x2 (toString (x-dx))
        , Svg.Attributes.y2 (toString (y-dy))
        , Svg.Attributes.style ("stroke:rgb(0,0,0);stroke-width:"++strokeWidth) ]
        []
