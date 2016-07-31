module Update.Particle exposing (stepParticles)

import Time exposing (Time)

import Model.Scene exposing (..)
import Model.Scene.Particle exposing (..)
import Model.Scene.Common exposing (..)


stepParticles : Time -> Scene -> Scene
stepParticles delta ({particles} as scene) =
  let
      particles' =
        particles
        |> List.map (stepParticle delta)
        |> List.filter (\p -> p.createdAt+particleLifetime > scene.absoluteTime)
  in
      { scene | particles = particles' }


stepParticle : Time -> Particle -> Particle
stepParticle delta ({position,velocity} as particle) =
  let
      velocity' = { velocity | y = velocity.y + gravityConstant * delta }
      position' =
        { x = position.x + delta*velocity'.x
        , y = position.y + delta*velocity'.y }
  in
      { particle | position = position'
                 , velocity = velocity' }
