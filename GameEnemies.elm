module GameEnemies where

type Formation = [{ x:Float, y:Float }]
type MovementPath = [{ x:Float, y:Float }]

squareFormation : Formation
squareFormation =
    [ { x = -40, y = -40 }
    , { x = -40, y = 0 }
    , { x = -40, y = 40 }
    , { x = 0,   y = 40 }
    , { x = 40,  y = 40 }
    , { x = 40,  y = 0 }
    , { x = 40,  y = -40 }
    , { x = 0,   y = -40 } ]

circleFormation : Formation
circleFormation =
    []

spiralFormation : Formation
spiralFormation =
    []

--linearPath : Float -> Float -> Float -> MovementPath
linearPath grad = --{t,speed,pos}
    []
    --{ pos | x <- pos.x + speed
          --, y <- pos.y + speed }

--createEnemyGroup : Formation -> MovementPath -> [Enemy]
createGroup formation movementPath =
    []

generate : Int -> (Formation, MovementPath)
generate difficulty = -- add seed here
    let formation' = squareFormation
        movementPath' = linearPath (pi/2 - pi/4)
    in ( formation'
       , movementPath' )