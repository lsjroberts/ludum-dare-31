module GameDraw where

type Sprite = { shape:Form, sides:Int, size:Int }

playerColour = rgb 170 255 0
enemyColour1 = rgb 255 170 0
enemyColour2 = rgb 255 0 170

polySideLength : Float -> Float -> Float
polySideLength sides radius =
    2 * radius * (sin (pi / sides))

player : Sprite
player =
    let sides' = 5
        size'  = 15
    in { shape = filled playerColour (ngon sides' size')
       , sides = sides'
       , size  = size' }

enemy1 : Sprite
enemy1 =
    let sides' = 3
        size'  = 10
    in { shape = filled enemyColour1 (ngon sides' size')
       , sides = sides'
       , size  = size' }

playerBullet1 : Sprite
playerBullet1 =
    let sideLen = (polySideLength (toFloat player.sides) (toFloat player.size))
    in { shape = filled playerColour (rect sideLen 2)
       , sides = 0
       , size  = 0 }

--enemyBullet1 : Enemy -> Sprite
enemyBullet1 enemy =
    let sideLen = (polySideLength (toFloat enemy.spr.sides) (toFloat enemy.spr.size))
    in { shape = filled enemyColour1 (rect sideLen 2)
       , sides = 0
       , size  = 0 }