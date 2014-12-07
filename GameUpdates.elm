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

near : Float -> Float -> Float -> Bool
near value compare offset =
    value >= compare - offset
 && value <= compare + offset

onCanvas : GameModel.Actor -> Bool
onCanvas ({pos} as actor) =
    near pos.x 0 (GameModel.halfWidth - 1)
 && near pos.y 0 (GameModel.halfHeight - 1)

movePos : Time -> GameModel.Position -> GameModel.Velocity -> GameModel.Position
movePos t pos vel =
    { x = clamp -GameModel.halfWidth GameModel.halfWidth (pos.x + vel.vx * t)
    , y = clamp -GameModel.halfHeight GameModel.halfHeight (pos.y + vel.vy * t) }

accelerate : Time -> Float -> Float -> Int -> Float -> Float
accelerate t accel speed dir vel =
    clamp -speed speed (vel + accel * toFloat dir)

actorPosition : Time -> GameModel.Actor -> GameModel.Position
actorPosition t actor =
    movePos t actor.pos actor.vel

actorVelocity : Time -> GameInput.Direction -> GameModel.Actor-> GameModel.Velocity
actorVelocity t dir ({vel,speed,accel,deccel} as actor) =
    { vx =  if dir.x == 0
                then tendTo vel.vx 0 deccel
                else accelerate t accel speed dir.x vel.vx
    , vy =  if dir.y == 0
                then tendTo vel.vy 0 deccel
                else accelerate t accel speed dir.y vel.vy }

actorRotationalVelocity : Time -> GameInput.Direction -> GameModel.Actor -> Float
actorRotationalVelocity t dir ({rot} as actor) =
    if dir.x == 0
        then tendTo rot.vel 0 rot.deccel
        else accelerate t rot.accel rot.speed -dir.x rot.vel

actorAngle : Time -> GameInput.Direction -> GameModel.Actor -> Float
actorAngle t dir ({rot} as actor) =
    let rotVel = actorRotationalVelocity t dir actor
    in rot.angle + rotVel

moveActor : Time -> GameInput.Direction -> GameModel.Actor -> GameModel.Actor
moveActor t dir actor =
    let pos' = actorPosition t actor
        vel' = actorVelocity t dir actor
    in { actor | pos <- pos'
               , vel <- vel' }

rotateActor : Time -> GameInput.Direction -> GameModel.Actor -> GameModel.Actor
rotateActor t dir ({rot} as actor) =
    let rot' = { rot | angle <- actorAngle t dir actor }
    in { actor | rot <- rot' }

--firePlayerGuns : Time -> Bool -> GameModel.Player -> [GameModel.PlayerBullet]
--firePlayerGuns t fire player =
--    player
    --sampleOn (fps 3) GameModel.createPlayerBullet

fireGun : GameModel.Gun -> GameModel.Gun
fireGun gun =
    { gun | timeSince <- 0 }

reloadGun : Time -> GameModel.Gun -> GameModel.Gun
reloadGun t gun =
    let timeSince' = gun.timeSince + t
    in { gun | timeSince <- timeSince' }

holdFire : GameModel.Actor -> GameModel.Actor
holdFire ({gun} as actor) =
    let gun' = { gun | firing <- False }
    in { actor | gun <- gun' }

stepGun : Time -> GameModel.Actor -> GameModel.Actor
stepGun t ({gun} as actor) =
    if gun.timeSince > (1/gun.fireRate) then { actor | gun <- fireGun gun }
                                        else { actor | gun <- reloadGun t gun }

stepPlayerGun : Time -> Bool -> GameModel.Actor -> GameModel.Actor
stepPlayerGun t fire ({gun} as actor) =
    let gun' = { gun | firing <- fire }
    in if fire then stepGun t { actor | gun <- gun' }
               else { actor | gun <- gun' }

stepPlayer : Time -> GameInput.UserInput -> GameModel.Player -> GameModel.Player
stepPlayer t input player =
    player |> moveActor t input.dir
           |> rotateActor t input.dir
           |> stepPlayerGun t input.fire1

stepPlayerBullets : Time -> GameInput.UserInput -> GameModel.Player -> [GameModel.PlayerBullet] -> [GameModel.PlayerBullet]
stepPlayerBullets t input ({gun} as player) bullets =
    bullets |> map (\bullet -> bullet |> moveActor t {x=0,y=0}
                                      |> rotateActor t {x=0,y=0} )
            |> filter onCanvas
            |> (++) (if gun.firing && gun.timeSince == 0
                        then player |> GameModel.createPlayerBullets
                        else [])
{--
stepEnemy : Time -> GameModel.Enemy -> GameModel.Enemy
stepEnemy t enemy =
    enemy |> moveActor t {x=2,y=2}

stepEnemyGroup : Time -> GameModel.EnemyGroup -> GameModel.EnemyGroup
stepEnemyGroup t ({enemies'} as enemyGroup) =
    { enemyGroup | enemies <- enemies' |> map(\enemy -> enemy |> stepEnemy t) }

stepEnemyGroups : Time -> [GameModel.EnemyGroup] -> [GameModel.EnemyGroup]
stepEnemyGroups t groups =
    groups |> map (\enemyGroup -> enemyGroup |> stepEnemyGroup t)
--}

stepGame : GameInput.Input -> GameModel.GameState -> GameModel.GameState
stepGame {timeDelta,userInput} ({player,playerBullets,enemies} as gameState) =
    let player'        = player |> stepPlayer timeDelta userInput
        playerBullets' = playerBullets |> stepPlayerBullets timeDelta userInput player
        --enemies'       = enemies |> stepEnemyGroups timeDelta
    in { gameState | player        <- player'
                   , playerBullets <- playerBullets'
                   , enemies       <- enemies }