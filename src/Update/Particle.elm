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
        |> List.filter (\particle -> isAlive scene.absoluteTime particle)
  in
      { scene | particles = particles' }


stepParticle : Time -> Particle -> Particle
stepParticle delta ({position,velocity} as particle) =
  let
      velocity' = { velocity | y = velocity.y + gravityConstant/10 * delta }
      position' =
        { x = position.x + delta*velocity'.x
        , y = position.y + delta*velocity'.y }
      angle' = particle.angle + delta * ((position.x + velocity.y)*0.01 |> sin)
  in
      { particle | position = position'
                 , velocity = velocity'
                 , angle = angle' }


isAlive : Time -> Particle -> Bool
isAlive absoluteTime particle =
  absoluteTime < particle.createdAt + particleLifetime && particle.position.y<=0
