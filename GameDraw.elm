module GameDraw where

type Sprite = { shape:Form, sides:Int, size:Int }

playerColour = rgb 170 255 0
playerColour2 = rgb 0 170 255
enemyColour1 = rgb 255 170 0
enemyColour2 = rgb 255 0 170

polySideLength : Float -> Float -> Float
polySideLength sides radius =
    2 * radius * (sin (pi / sides))

--spritePolygon sides size =
--    let sideLen = (polySideLength (toFloat sides) (toFloat size))
--    in collage size size
--        [ ngon sides size |> filled playerColour
--        , rect sideLen 2 |> filled enemyColour1 |> move (-10, -10) ]

player : Sprite
player =
    let sides' = 6
        size'  = 15
    in { shape = ngon sides' size' |> filled playerColour --spritePolygon sides' size'
       , sides = sides'
       , size  = size' }

playerSides : Int -> Sprite
playerSides sides' =
    let p = player
    in { p | shape <- ngon sides' (toFloat p.size) |> filled playerColour
           , sides <- sides' }

playerLife : Sprite
playerLife =
    let sides' = 5
        size'  = 15
    in { shape = ngon sides' size' |> filled playerColour2 --spritePolygon sides' size'
       , sides = sides'
       , size  = size' }

enemy1 : Sprite
enemy1 =
    let sides' = 3
        size'  = 10
    in { shape = ngon sides' size' |> filled enemyColour1
       , sides = sides'
       , size  = size' }

playerBullet1 : Sprite
playerBullet1 =
    {--}
    let sideLen = (polySideLength (toFloat player.sides) (toFloat player.size))
    in { shape = filled playerColour (rect sideLen 4)
       , sides = 0
       , size  = floor 4 }
    --}
    {--
    { shape = circle 4 |> filled playerColour
    , sides = 0
    , size = 4 }
    --}

--enemyBullet1 : Enemy -> Sprite
enemyBullet1 enemy =
    let sideLen = (polySideLength (toFloat enemy.spr.sides) (toFloat enemy.spr.size))
    in { shape = circle (toFloat enemy.spr.sides + 4) |> filled enemyColour2 --shape = filled enemyColour1 (rect sideLen 2)
       , sides = 0
       , size  = 0 }