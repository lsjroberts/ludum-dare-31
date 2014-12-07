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
linearPath m x c =
    []

--createEnemyGroup : Formation -> MovementPath -> [Enemy]
createGroup formation movementPath =
    []

generate : Int -> (Formation, MovementPath)
generate difficulty = -- add seed here
    let formation' = squareFormation
        movementPath' = linearPath 0.5 0 0
    in ( formation'
       , movementPath' )