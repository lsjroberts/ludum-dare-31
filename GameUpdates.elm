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

accelerate : Time -> Float -> Float -> Float -> Float -> Float
accelerate t accel speed dir vel =
    clamp -speed speed (vel + accel * dir)

actorPosition : Time -> GameModel.Actor -> GameModel.Position
actorPosition t actor =
    movePos t actor.pos actor.vel

actorVelocity : Time -> GameModel.Direction -> GameModel.Actor-> GameModel.Velocity
actorVelocity t dir ({vel,speed,accel,deccel} as actor) =
    { vx =  if dir.x == 0
                then tendTo vel.vx 0 deccel
                else accelerate t accel speed dir.x vel.vx
    , vy =  if dir.y == 0
                then tendTo vel.vy 0 deccel
                else accelerate t accel speed dir.y vel.vy }

actorRotationalVelocity : Time -> GameModel.Direction -> GameModel.Actor -> Float
actorRotationalVelocity t dir ({rot} as actor) =
    if dir.x == 0
        then tendTo rot.vel 0 rot.deccel
        else accelerate t rot.accel rot.speed -dir.x rot.vel

actorAngle : Time -> GameModel.Direction -> GameModel.Actor -> Float
actorAngle t dir ({rot} as actor) =
    let rotVel = actorRotationalVelocity t dir actor
    in rot.angle + rotVel

moveActor : Time -> GameModel.Direction -> GameModel.Actor -> GameModel.Actor
moveActor t dir actor =
    let pos' = actorPosition t actor
        vel' = actorVelocity t dir actor
    in { actor | pos <- pos'
               , vel <- vel' }

rotateActor : Time -> GameModel.Direction -> GameModel.Actor -> GameModel.Actor
rotateActor t dir ({rot} as actor) =
    let rot' = { rot | angle <- actorAngle t dir actor }
    in { actor | rot <- rot' }

fireGun : GameModel.Gun -> GameModel.Gun
fireGun gun =
    { gun | timeSince <- 0 }

reloadGun : Time -> GameModel.Gun -> GameModel.Gun
reloadGun t gun =
    let timeSince' = gun.timeSince + t
    in { gun | timeSince <- timeSince' }

stepGun : Time -> GameModel.Actor -> GameModel.Actor
stepGun t ({gun} as actor) =
    if gun.timeSince > (1/gun.fireRate) then { actor | gun <- fireGun gun }
                                        else { actor | gun <- reloadGun t gun }

stepBullets : Time -> [GameModel.Bullet] -> [GameModel.Bullet]
stepBullets t bullets =
    bullets |> map (\bullet -> bullet |> moveActor t {x=0,y=0}
                                      |> rotateActor t {x=0,y=0} )
            |> filter onCanvas

stepPlayerGun : Time -> Bool -> GameModel.Actor -> GameModel.Actor
stepPlayerGun t fire ({gun} as actor) =
    let gun' = { gun | firing <- fire }
    in if fire then stepGun t { actor | gun <- gun' }
               else { actor | gun <- gun' }

stepPlayer : Time -> GameInput.UserInput -> GameModel.Player -> GameModel.Player
stepPlayer t ({dir} as input) player =
    let dir' = { dir | x <- toFloat dir.x
                     , y <- toFloat dir.y }
    in player |> moveActor t dir'
              |> rotateActor t dir'
              |> stepPlayerGun t input.fire1

stepPlayerBullets : Time -> GameInput.UserInput -> GameModel.Player -> [GameModel.Bullet] -> [GameModel.Bullet]
stepPlayerBullets t input ({gun} as player) bullets =
    bullets |> stepBullets t
            |> (++) (if gun.firing && gun.timeSince == 0
                        then player |> GameModel.createPlayerBullets
                        else [])

--getNextPathVelocity t movementPath actor =
--    let pos = movementPath t actor.pos
--    in { actor | vel <- { vx = }}

stepEnemy : Time -> GameModel.Velocity -> GameModel.Enemy -> GameModel.Enemy
stepEnemy t nextFormationVel enemy =
    let dir = { x = nextFormationVel.vx
              , y = nextFormationVel.vy }
    in enemy |> moveActor t dir
             |> stepGun t

stepEnemyGroup : Time -> GameModel.EnemyGroup -> GameModel.EnemyGroup
stepEnemyGroup t ({enemies,movementPath,pos} as enemyGroup) =
    let nextFormationVel = {vx=0,vy=-1}--movementPath {t=t,speed=speed,pos=pos}
    in { enemyGroup | enemies <- enemies |> map(\enemy -> enemy |> stepEnemy t nextFormationVel) }

stepEnemyGroups : Time -> [GameModel.EnemyGroup] -> [GameModel.EnemyGroup]
stepEnemyGroups t groups =
    groups |> map (\enemyGroup -> enemyGroup |> stepEnemyGroup t)

fireEnemyBullets : Time -> GameModel.Enemy -> [GameModel.Bullet]
fireEnemyBullets t ({gun} as enemy) =
    if gun.timeSince == 0 then GameModel.createEnemyBullets enemy
                          else []

fireEnemyGroupBullets : Time -> GameModel.EnemyGroup -> [GameModel.Bullet]
fireEnemyGroupBullets t ({enemies} as enemyGroup) =
    enemies |> concatMap (\enemy -> enemy |> fireEnemyBullets t)

stepEnemyGroupsBullets : Time -> [GameModel.EnemyGroup] -> [GameModel.Bullet] -> [GameModel.Bullet]
stepEnemyGroupsBullets t enemyGroups bullets =
    bullets |> stepBullets t
            |> (++) (enemyGroups |> concatMap (\enemyGroup -> enemyGroup |> fireEnemyGroupBullets t))

stepGame : GameInput.Input -> GameModel.GameState -> GameModel.GameState
stepGame {timeDelta,userInput} ({player,playerBullets,enemies,enemyBullets} as gameState) =
    let player' = player |> stepPlayer timeDelta userInput
        playerBullets' = playerBullets |> stepPlayerBullets timeDelta userInput player
        enemies' = enemies |> stepEnemyGroups timeDelta
        enemyBullets' = enemyBullets |> stepEnemyGroupsBullets timeDelta enemies
    in { gameState | player        <- player'
                   , playerBullets <- playerBullets'
                   , enemies       <- enemies'
                   , enemyBullets  <- enemyBullets' }