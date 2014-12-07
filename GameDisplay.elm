module GameDisplay where

import Debug

import GameModel as GameModel
import GameEnemies as GameEnemies
import GameDraw as GameDraw

fixWeirdAngles : GameModel.Actor -> Form -> Form
fixWeirdAngles ({spr,rot} as actor) shape =
    if | spr.sides > 2 -> rotate (rot.angle + pi/(toFloat spr.sides)) shape
       | otherwise -> rotate (rot.angle + pi/2) shape

displayActor : GameModel.Actor -> Form
displayActor ({spr,pos,rot} as actor) =
    spr.shape |> move (pos.x, pos.y)
              --|> rotate (rot.angle)
              |> fixWeirdAngles actor

displayActors : [GameModel.Actor] -> [Form]
displayActors actors =
    actors |> map (\actor -> actor |> displayActor)

displayEnemyGroups : [GameModel.EnemyGroup] -> [Form]
displayEnemyGroups groups =
    groups |> concatMap (\group -> group.enemies |> displayActors)

display : (Int,Int) -> GameModel.GameState -> Element
display (w,h) ({player,playerBullets,enemies} as gameState) =
    let displayedActors =
            displayEnemyGroups enemies ++
            displayActors playerBullets ++
            [ displayActor player ]
    in container w h middle <| collage w h
        displayedActors