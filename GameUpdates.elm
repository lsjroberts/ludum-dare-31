module GameUpdates where

import Pseudorandom as R

import GameModel as GameModel
import GameInput as GameInput
import GameEnemies as GameEnemies
import GameDraw as GameDraw

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
    value >= compare - offset && value <= compare + offset

outside : Float -> Float -> Float -> Bool
outside value compare offset =
    value <= compare - offset || value >= compare + offset

isTrue : Bool -> Bool
isTrue val =
    if val then True else False

isFalse : Bool -> Bool
isFalse val =
    if val then False else True

onCanvas : GameModel.Actor -> Bool
onCanvas ({pos} as actor) =
    near pos.x 0 (GameModel.halfWidth - 1)
 && near pos.y 0 (GameModel.halfHeight - 1)

dist : GameModel.Position -> GameModel.Position -> Float
dist pos pos' =
    sqrt <| (pos.x - pos'.x)^2 + (pos.y - pos'.y)^2

unitVector : GameModel.Position -> GameModel.Position
unitVector pos =
    let m = sqrt <| pos.x^2 + pos.y^2
    in { pos | x <- pos.x / m
             , y <- pos.y / m }

unitVectorTwoPoints : GameModel.Position -> GameModel.Position -> GameModel.Position
unitVectorTwoPoints pos pos' =
    let v = { x = pos.x - pos'.x
            , y = pos.y - pos'.y }
    in unitVector v

collides : GameModel.Actor -> GameModel.Actor -> Bool
collides actor actor' =
    if (dist actor.pos actor'.pos) < (toFloat actor.spr.size + toFloat actor'.spr.size) then True else False

collider : [GameModel.Actor] -> GameModel.Actor -> Bool
collider collideActors actor =
    let len = length <| filter isTrue (map (\ca -> collides ca actor) collideActors)
    in if len > 0 then True else False

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

--actorVelocityAvoidEdges : Time -> GameModel.Direction -> GameModel.Actor-> GameModel.Velocity
--actorVelocityAvoidEdges t dir ({pos,vel,speed,accel,deccel} as actor) =
--    if | isFalse <| near pos.x 0 (GameModel.halfWidth - 100) -> { vel | vx <- 0-vel.vx }
--       | isFalse <| near pos.y 0 (GameModel.halfHeight - 10) -> { vel | vy <- 0-vel.vy }
--       | otherwise -> actorVelocity t dir actor

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

--moveActorAvoidEdges : Time -> GameModel.Direction -> GameModel.Actor -> GameModel.Actor
--moveActorAvoidEdges t dir actor =
--    let pos' = actorPosition t actor
--        vel' = actorVelocityAvoidEdges t dir actor
--    in { actor | pos <- pos'
--               , vel <- vel' }

rotateActor : Time -> GameModel.Direction -> GameModel.Actor -> GameModel.Actor
rotateActor t dir ({rot} as actor) =
    let rot' = { rot | angle <- actorAngle t dir actor }
    in { actor | rot <- rot' }

stepActorLives : Time -> [GameModel.Bullet] -> GameModel.Actor -> GameModel.Actor
stepActorLives t bullets actor =
    let bulletsCollider = collider bullets
    in if bulletsCollider actor then { actor | lives <- actor.lives - 1
                                             , loseLifeCooldown <- actor.loseLifeCooldownMax }
                                else actor

stepActorLivesCooldown : Time -> GameModel.Actor -> GameModel.Actor
stepActorLivesCooldown t ({loseLifeCooldown,loseLifeCooldownMax} as actor) =
    if loseLifeCooldown <= 0 then { actor | loseLifeCooldown <- 0 }
                             else { actor | loseLifeCooldown <- loseLifeCooldown - t }

stepActorLivesWithCooldown : Time -> [GameModel.Bullet] -> GameModel.Actor -> GameModel.Actor
stepActorLivesWithCooldown t bullets actor =
    if actor.loseLifeCooldown <= 0 then stepActorLives t bullets actor
                                   else actor

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

stepPlayerSpriteLivesSides : GameModel.Player -> GameModel.Player
stepPlayerSpriteLivesSides player =
    if (player.lives + 2) == player.spr.sides then player
                                        else { player | spr <- GameDraw.playerSides (player.lives + 2) }

stepPlayer : Time -> GameInput.UserInput -> [GameModel.Bullet] -> Int -> GameModel.Player -> GameModel.Player
stepPlayer t ({dir,rot} as input) enemyBullets score player =
    let dir' = { dir | x <- toFloat dir.x
                     , y <- toFloat dir.y }
        rot' = { x = toFloat rot, y = 0 }
    in player |> moveActor t dir'
              |> rotateActor t rot'
              |> stepPlayerGun t input.fire1
              |> stepActorLivesCooldown t
              |> stepActorLivesWithCooldown t enemyBullets
              |> stepPlayerSpriteLivesSides

stepPlayerBullets : Time -> GameInput.UserInput -> GameModel.Player -> [GameModel.Bullet] -> [GameModel.Bullet]
stepPlayerBullets t input ({gun} as player) bullets =
    bullets |> stepBullets t
            |> (++) (if gun.firing && gun.timeSince == 0
                        then player |> GameModel.createPlayerBullets
                        else [])

killEnemy : [GameModel.Bullet] -> GameModel.Enemy -> GameModel.Enemy
killEnemy playerBullets enemy =
    if enemy |> collider playerBullets then { enemy | kill <- True }
                                       else enemy

avoidEdgesActor t ({dir,pos} as actor) =
    let flipDirX = { dir | x <- 0-dir.x }
        flipDirY = { dir | y <- 0-dir.y }
        flipPosX = { pos | x <- 0 }
        flipPosY = { pos | y <- 0 }
    in if | outside pos.x 0 (GameModel.halfWidth - 10) -> { actor | pos <- flipPosX }
          | outside pos.y 0 (GameModel.halfHeight - 10) -> { actor | pos <- flipPosY }
          | otherwise -> actor
    --in actor

stepEnemy : Time -> GameModel.Player -> [GameModel.Bullet] -> GameModel.Enemy -> GameModel.Enemy
stepEnemy t player playerBullets enemy =
    let dir = unitVectorTwoPoints player.pos enemy.pos
    in enemy |> avoidEdgesActor t
             |> moveActor t enemy.dir
             |> rotateActor t enemy.dir
             |> stepGun t
             |> killEnemy playerBullets

isActorKilled actor =
    actor.kill

spawnEnemyBatch : GameModel.Seeds -> Float -> GameModel.Player -> [GameModel.Enemy] -> [GameModel.Enemy]
spawnEnemyBatch seeds spawnCooldown player enemies =
    --let spawnX = clamp -GameModel.halfWidth GameModel.halfWidth (player.pos.x + Random.range -300 300)
        --spawnY = clamp -GameModel.halfHeight GameModel.halfHeight (player.pos.y + Random.range -300 300)
    let pos = { x = player.pos.x + (100 * toFloat (R.get seeds.a <| R.range (-3,3)))
              , y = player.pos.y + (100 * toFloat (R.get seeds.b <| R.range(-3,3))) }
        dir = { x = (R.get seeds.c <| R.float) - (R.get seeds.a <| R.float)
              , y = (R.get seeds.d <| R.float) - (R.get seeds.b <| R.float) }
        formation = GameEnemies.squareFormation 3 40
    in if spawnCooldown <= 0
        then (GameModel.createEnemyBatch formation pos dir) ++ enemies
        else enemies

stepEnemies : Time -> GameModel.Seeds ->GameModel.Player -> [GameModel.Bullet] -> Float -> [GameModel.Enemy] -> [GameModel.Enemy]
stepEnemies t seeds player playerBullets spawnCooldown enemies =
    enemies |> filter (isActorKilled >> not)
            |> spawnEnemyBatch seeds spawnCooldown player
            |> map (\enemy -> enemy |> stepEnemy t player playerBullets)

--moveEnemiesInGroup t seed player playerBullets spawnCooldown ({enemies} as enemyGroup) =
--    { enemyGroup | enemies <- enemies |> stepEnemies seed t player playerBullets spawnCooldown  }

--stepEnemyGroups t seed player playerBullets spawnCooldown groups =
--    groups |> map (\enemyGroup -> enemyGroup |> moveEnemiesInGroup t seed)

fireEnemyBullets : Time -> GameModel.Enemy -> [GameModel.Bullet]
fireEnemyBullets t ({gun} as enemy) =
    if gun.timeSince == 0 then GameModel.createEnemyBullets enemy
                          else []

stepEnemyBullets : Time -> [GameModel.Enemy] -> [GameModel.Bullet] -> [GameModel.Bullet]
stepEnemyBullets t enemies bullets =
    bullets |> stepBullets t
            |> (++) (enemies |> concatMap (\enemy -> enemy |> fireEnemyBullets t))

--addScoreForKill : GameModel.Enemy -> Int
addScoreForKill enemy acc =
    if enemy.kill then acc + 100 else acc

stepScore : Time -> [GameModel.Enemy] -> Int -> Int
stepScore t enemies score =
    foldl addScoreForKill score enemies

stepSpawnCooldown : Time -> Int -> Float -> Float
stepSpawnCooldown t score spawnCooldown =
    if spawnCooldown <= 0 then 1
                          else spawnCooldown - t

stepUI : Time -> GameModel.Player -> GameModel.UI -> GameModel.UI
stepUI t player ui =
    ui
    --if (length ui.lives == player.lives) then ui
                                         --else { ui | lives <- GameModel.createPlayerLivesUI player.lives }

stepGame : GameInput.Input -> GameModel.GameState -> GameModel.GameState
stepGame {timeDelta,userInput} ({state,player,playerBullets,enemies,enemyBullets,score,spawnCooldown,ui,seeds} as gameState) =
    if | state == GameModel.Pause && userInput.unpause -> { gameState | state <- GameModel.Play }
       | state == GameModel.Pause -> gameState
       | state == GameModel.Lost -> gameState
       | player.lives <= 0 -> { gameState | state <- GameModel.Lost }
       | otherwise ->
            let player' = player |> stepPlayer timeDelta userInput enemyBullets score
                playerBullets' = playerBullets |> stepPlayerBullets timeDelta userInput player
                enemies' = enemies |> stepEnemies timeDelta seeds player playerBullets spawnCooldown
                enemyBullets' = enemyBullets |> stepEnemyBullets timeDelta enemies
                score' = score |> stepScore timeDelta enemies
                spawnCooldown' = spawnCooldown |> stepSpawnCooldown timeDelta score
                ui' = ui |> stepUI timeDelta player
                seeds' = {a=R.get seeds.a R.int,b=R.get seeds.b R.int,c=R.get seeds.c R.int,d=R.get seeds.d R.int}
            in { gameState | player        <- player'
                           , playerBullets <- playerBullets'
                           , enemies       <- enemies'
                           , enemyBullets  <- enemyBullets'
                           , score         <- score'
                           , spawnCooldown <- spawnCooldown'
                           , ui            <- ui'
                           , seeds         <- seeds' }