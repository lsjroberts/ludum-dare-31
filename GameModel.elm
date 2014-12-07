module GameModel where

import Random

import GameDraw as GameDraw
import GameEnemies as GameEnemies

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
type Bullet = Actor
type PlayerBullet = Bullet
type EnemyGroup = { enemies:[Enemy]
                  , formation:GameEnemies.Formation
                  , movementPath:GameEnemies.MovementPath }

type GameState = { player:Player
                 , playerBullets:[PlayerBullet]
                 , enemies:[EnemyGroup] }

(gameWidth, gameHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

createActor : Float -> Float -> GameDraw.Sprite -> Actor
createActor x y spr =
    { pos      = { x = x, y = y }
    , vel      = { vx = 0, vy = 0 }
    , rot      = { angle = 0, speed = 0, vel = 0, accel = 0, deccel = 0 }
    , speed    = 0
    , accel    = 0
    , deccel   = 0
    , spr      = spr }

createPlayer : Float -> Float -> Player
createPlayer x y =
    let actor = createActor x y GameDraw.player
    in { actor | rot    <- { angle  = 0
                           , speed  = pi / 4
                           , vel    = 0
                           , accel  = pi / 14
                           , deccel = pi / 20 }
               , speed  <- 200
               , accel  <- 40
               , deccel <- 5 }

setActorBulletAngle : Float -> Bullet -> Bullet
setActorBulletAngle angle' ({rot} as bullet) =
    let rot' = { rot | angle <- angle' }
    in { bullet | rot <- rot' }

setActorBulletVelocity : Float -> Float -> Int -> Bullet -> Bullet
setActorBulletVelocity angle' speed' sides ({vel} as bullet) =
    let vel' = { vel | vx <- speed' * cos (angle')
                     , vy <- speed' * sin (angle') }
    in { bullet | vel <- vel'
                , speed <- speed' }

createActorBullet : Actor -> GameDraw.Sprite -> Float -> Int -> Bullet
createActorBullet ({pos} as actor) spr speed' side =
    let angle' = actor.rot.angle + (2 * pi * (toFloat side) / (toFloat actor.spr.sides))
        bullet = createActor pos.x pos.y spr
    in bullet |> setActorBulletAngle angle'
              |> setActorBulletVelocity angle' speed' actor.spr.sides

createActorBullets : Actor -> GameDraw.Sprite -> Float -> [Bullet]
createActorBullets actor spr speed =
    let num = actor.spr.sides
    in [1..num] |> map(\n -> createActorBullet actor spr speed n)

createPlayerBullets : Player -> [PlayerBullet]
createPlayerBullets ({pos,vel,rot} as player) =
    createActorBullets player GameDraw.playerBullet1 400

createEnemies : (GameEnemies.Formation, GameEnemies.MovementPath) -> EnemyGroup
createEnemies (formation', movementPath') =
    let enemies' = []
    in { enemies = enemies'
       , formation = formation'
       , movementPath = movementPath' }

defaultGame : GameState
defaultGame = { player = createPlayer 0 0
              , playerBullets = []
              , enemies = [ createEnemies <| GameEnemies.generate 1 ] }