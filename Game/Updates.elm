module Updates where

import Model (halfWidth,halfHeight)

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

actorVelocity t actor dir =
    { vx = accelerate t actor.accel actor.speed dir.x actor.vel.vx
    , vy = accelerate t actor.accel actor.speed dir.y actor.vel.vy }

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