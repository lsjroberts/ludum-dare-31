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
    --          |> rotate (rot.angle)
              |> fixWeirdAngles actor

displayActors : [GameModel.Actor] -> [Form]
displayActors actors =
    actors |> map (\actor -> actor |> displayActor)

display : (Int,Int) -> GameModel.GameState -> Element
display (w,h) ({player,playerBullets,enemies,enemyBullets,ui} as gameState) =
    let displayedActors =
            [ rect (toFloat w) (toFloat h) |> filled black ] ++
            displayActors enemies ++
            displayActors playerBullets ++
            displayActors enemyBullets ++
            displayActors ui.lives ++
            [ displayActor player ]
    in container w h middle <| collage w h
        displayedActors