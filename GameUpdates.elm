module GameUpdates where

import GameModel as GameModel
import GameInput as GameInput

tendTo current target step =
    if | current > target -> current - step
       | current < target -> current + step
       | otherwise -> target

tendToClose current target step give =
    if | current > (target - give) -> current - step
       | current < (target + give) -> current + step
       | otherwise -> target

movePos : Time -> GameModel.Position -> GameModel.Velocity -> GameModel.Position
movePos t pos vel =
    { x = clamp -GameModel.halfWidth GameModel.halfWidth (pos.x + vel.vx * t)
    , y = clamp -GameModel.halfHeight GameModel.halfHeight (pos.y + vel.vy * t) }

accelerate : Time -> Float -> Float -> Int -> Float -> Float
accelerate t accel speed dir vel =
    clamp -speed speed (vel + accel * toFloat dir)

actorPosition t actor =
    movePos t actor.pos actor.vel

actorVelocity t ({vel,speed,accel,deccel} as actor) dir =
    { vx =  if dir.x == 0
                then tendTo vel.vx 0 deccel
                else accelerate t accel speed dir.x vel.vx
    , vy =  if dir.y == 0
                then tendTo vel.vy 0 deccel
                else accelerate t accel speed dir.y vel.vy }

playerRotationalVelocity t ({rot} as player) dir =
    if dir.x == 0
        then tendTo rot.vel 0 rot.deccel
        else accelerate t rot.accel rot.speed -dir.x rot.vel

playerAngle t ({rot} as player) dir =
    let rotVel1 = playerRotationalVelocity t player dir
    in rot.angle + rotVel1

stepPlayer : Time -> GameInput.UserInput -> GameModel.Player -> GameModel.Player
stepPlayer t input ({pos,vel,rot} as player) =
    let pos1    = actorPosition t player
        vel1    = actorVelocity t player input.dir
        angle   = playerAngle t player input.dir
    in { player | pos <- pos1
                , vel <- vel1
                , rot <- { rot | angle <- angle } }

stepGame : GameInput.Input -> GameModel.GameState -> GameModel.GameState
stepGame {timeDelta,userInput} ({player} as gameState) =
    { gameState | player <- stepPlayer timeDelta userInput player }