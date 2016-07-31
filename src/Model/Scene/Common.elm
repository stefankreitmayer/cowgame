module Model.Scene.Common exposing (..)

import Time exposing (Time)


gravityConstant : Float
gravityConstant =
  0.00007


particleLifetime : Time
particleLifetime = 2000


isUnderUdder : Float -> Bool
isUnderUdder posX =
  posX > 0.78 && posX < 0.9


udderStartX : Float
udderStartX = 0.78


udderEndX : Float
udderEndX = 0.9
