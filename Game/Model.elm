module Model where

import Draw as Draw

{-- Model ---------------------------------------------------------------------

------------------------------------------------------------------------------}

type Position = { x:Float, y:Float }
type Velocity = { vx:Float, vy:Float }
type RotationalVelocity = { rx:Float, ry:Float }

type Actor = { pos:Position
             , vel:Velocity
             , rotVel:RotationalVelocity
             , speed:Float
             , accel:Float
             , rotSpeed:Float
             , rotAccel:Float
             , spr:Draw.Sprite }
type Player = Actor
type Enemy = Actor

type GameState = { player: Player }

(gameWidth, gameHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

createActor : Float -> Float -> Draw.Sprite -> Actor
createActor x y spr =
    { pos      = { x = x
                 , y = y }
    , vel      = { vx = 0
                 , vy = 0 }
    , rotVel   = { rx = 0
                 , ry = 0 }
    , speed    = 5.0
    , accel    = 0.1
    , rotSpeed = 5.0
    , rotAccel = 0.1
    , spr      = spr }

createPlayer : Float -> Float -> Player
createPlayer x y =
    let spr = Draw.player
    in createActor x y spr

defaultGame : GameState
defaultGame = { player = createPlayer 0 0
              }