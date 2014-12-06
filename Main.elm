module Main where

import Debug
import Keyboard
import Window

import Draw


{-- Input ---------------------------------------------------------------------

------------------------------------------------------------------------------}

type Direction = { x:Int, y:Int }
type UserInput = { dir:Direction }

userInput : Signal UserInput
userInput =
    let dir = Keyboard.arrows
    in (UserInput <~ dir)

type Input = { timeDelta:Float, userInput:UserInput }



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
             , deccel:Float
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
    , speed    = 200
    , accel    = 40
    , deccel   = 5
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



{-- Updates -------------------------------------------------------------------

------------------------------------------------------------------------------}
{--}

movePos : Time -> Position -> Velocity -> Position
movePos t pos vel =
    { x = clamp -halfWidth halfWidth (pos.x + vel.vx * t)
    , y = clamp -halfHeight halfHeight (pos.y + vel.vy * t) }

accelerate : Time -> Float -> Float -> Int -> Float -> Float
accelerate t accel speed dir vel =
    clamp -speed speed (vel + accel * toFloat dir)

actorPosition t actor =
    movePos t actor.pos actor.vel

actorVelocity t ({vel,speed,accel,deccel} as actor) dir =
    { vx = if dir.x == 0 then if vel.vx > 0 then vel.vx - deccel
                                            else vel.vx + deccel
                         else accelerate t accel speed dir.x vel.vx
    , vy = if dir.y == 0 then if vel.vy > 0 then vel.vy - deccel
                                            else vel.vy + deccel
                         else accelerate t accel speed dir.y vel.vy }

stepPlayer : Time -> UserInput -> Player -> Player
stepPlayer t input ({pos,vel} as player) =
    let pos1 = actorPosition t player
        vel1 = actorVelocity t player input.dir
    in { player | pos <- pos1
                , vel <- vel1 }

{--}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} ({player} as gameState) =
    { gameState | player <- stepPlayer timeDelta userInput player }
    --gameState



{-- Display -------------------------------------------------------------------

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) ({player} as gameState) =
    container w h middle <| collage gameWidth gameHeight
        --(Draw.display [ player.spr.shape ])
        [ move (player.pos.x, player.pos.y) player.spr.shape ]



{-- And put it together -------------------------------------------------------

------------------------------------------------------------------------------}

delta = inSeconds <~ fps 30
input = Debug.watch "input" <~ sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
