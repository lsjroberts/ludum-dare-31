module GameModel where

import Random

import GameDraw as GameDraw
import GameEnemies as GameEnemies

type Position = { x:Float, y:Float }
type Direction = { x:Float, y:Float }
type Velocity = { vx:Float, vy:Float }
type Rotation = { angle:Float, speed:Float, vel:Float, accel:Float, deccel:Float }
type Gun = { firing:Bool, fireRate:Float, timeSince:Float }

type Actor = { pos:Position
             , vel:Velocity
             , rot:Rotation
             , speed:Float
             , accel:Float
             , deccel:Float
             , spr:GameDraw.Sprite
             , gun:Gun
             , lives:Int
             , kill:Bool }
type Player = Actor
type Enemy = Actor
type Bullet = Actor
type EnemyGroup = { enemies:[Enemy]
                  , formation:GameEnemies.Formation
                  , target:Position }
type UI = { lives:[Actor] }

type Level = { name:String, enemies:[Enemy] }
type Levels = [Level]

data State = Play | Pause | Won | Lost

type GameState = { state:State
                 , player:Player
                 , playerBullets:[Bullet]
                 , enemies:[Enemy]
                 , enemyBullets:[Bullet]
                 , score:Int
                 , spawnCooldown:Float
                 , ui:UI }

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
    , spr      = spr
    , gun      = { firing = False, fireRate = 0, timeSince = 0 }
    , lives    = 1
    , kill     = False }

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
               , deccel <- 5
               , gun    <- { firing = False
                           , fireRate = 12
                           , timeSince = 0 }
               , lives  <- 3 }

createEnemy : Float -> Float -> Enemy
createEnemy x y =
    let actor = createActor x y GameDraw.enemy1
    in { actor | rot    <- { angle  = 0
                           , speed  = pi / 4
                           , vel    = pi / 10
                           , accel  = pi / 14
                           , deccel = pi / 20 }
               , speed  <- 40
               , accel  <- 5
               , deccel <- 5
               , gun    <- { firing = True
                           , fireRate = 0.534 --0.534 --1.067 --2.134
                           , timeSince = 1 } }

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

createPlayerBullets : Player -> [Bullet]
createPlayerBullets player =
    createActorBullets player GameDraw.playerBullet1 600

createEnemyBullets : Enemy -> [Bullet]
createEnemyBullets enemy =
    createActorBullets enemy (GameDraw.enemyBullet1 enemy) 300

createPlayerLivesUI : Int -> [Actor]
createPlayerLivesUI num =
    let (x,y) = (-halfWidth+10,halfHeight-10)
        step = 60
    in [1..num] |> map (\n -> createActor ((toFloat x) + (toFloat (n-1)) * step)
                                          (toFloat y)
                                          GameDraw.playerLife )

defaultGame : GameState
defaultGame =
    let player' = createPlayer 0 0
    in { state = Pause
       , player = player'
       , playerBullets = []
       , enemies = []
       , enemyBullets = []
       , score = 0
       , spawnCooldown = 0
       , ui = { lives = createPlayerLivesUI player'.lives } }