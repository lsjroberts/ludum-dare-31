module GameEnemies where

type Formation = [Position]
type MovementPath = [Position]
type EnemyGroup = { formation:Formation, movementPath:MovementPath }

squareFormation : Formation
squareFormation =
    []

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