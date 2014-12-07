module GameEnemies where

type Formation = [{ x:Float, y:Float }]
--type MovementPath = [{ x:Float, y:Float }]

modVal divisor val =
    if val % divisor == 0 || val == 0 then True else False

squareFormation : Float -> Float -> Formation
squareFormation len spacing =
    {--
    let minVal = -spacing * (toFloat <| floor (len/2))
        maxVal =  spacing * (toFloat <| floor (len/2))
        filterSpacing = modVal spacing
    --in []
    --in [minVal..maxVal] |> map (\v -> { x = minVal, y = v })
    in (filter filterSpacing [minVal..maxVal]) |>
        concatMap (\v -> (filter filterSpacing [minVal..maxVal]) |>
            map (\v2 -> {x=v,y=v2}))
    -- [minVal..maxVal] |> map (\v2 -> { x = v, y = v2 } ) )
    --        |> []

    --[1..4] |>
    --    map (\n -> n |>
    --        ([1..(len-1)] |>
    --            map (\m -> m |>
    --                { x = n})))
    --}
    [ { x = -spacing, y = -spacing }
    , { x = -spacing, y = 0 }
    , { x = -spacing, y = spacing }
    , { x = 0,   y = spacing }
    , { x = spacing,  y = spacing }
    , { x = spacing,  y = 0 }
    , { x = spacing,  y = -spacing }
    , { x = 0,   y = -spacing } ]

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

generate : Float -> Float -> Formation
generate difficulty spacing = -- add seed here
    squareFormation (difficulty+2) spacing
    --let formation' = squareFormation (difficulty+2) 40
        --movementPath' = linearPath (pi/2 - pi/4)
    --in ( formation'
       --, movementPath' )