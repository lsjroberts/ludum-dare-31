module GameModel where

import GameDraw

type Position = { x:Float, y:Float }
type Velocity = { vx:Float, vy:Float }
type Rotation = { angle:Float, speed:Float, vel:Float, accel:Float, deccel:Float }

type Actor = { pos:Position
             , vel:Velocity
             , rot:Rotation
             , speed:Float
             , accel:Float
             , deccel:Float
             , spr:GameDraw.Sprite }
type Player = Actor
type Enemy = Actor

type GameState = { player: Player }

(gameWidth, gameHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

createActor : Float -> Float -> GameDraw.Sprite -> Actor
createActor x y spr =
    { pos      = { x = x
                 , y = y }
    , vel      = { vx = 0
                 , vy = 0 }
    , rot      = { angle = 0
                 , speed  = pi / 10
                 , vel    = 0
                 , accel  = pi / 20
                 , deccel = pi / 20 }
    , speed    = 200
    , accel    = 40
    , deccel   = 5
    , spr      = spr }

createPlayer : Float -> Float -> Player
createPlayer x y =
    let spr = GameDraw.player
    in createActor x y spr

defaultGame : GameState
defaultGame = { player = createPlayer 0 0
              }