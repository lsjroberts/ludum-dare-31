module GameDisplay where

import Debug
import Text

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

txt f = toText >> Text.color GameDraw.playerColour2 >> f >> centered

displayScore score =
    toForm (txt (Text.height 40) (show score))
           |> move (0, GameModel.gameHeight/2)

title = "BUGS ARE HELL"
msg = "SPACE to start, AD to rotate, &uarr;&darr;&larr;&rarr; to move"

displayStatus state =
    if state == GameModel.Pause then [ toForm (txt (Text.height 25) (identity msg)) |> move (0, -80)
                                     , toForm (txt (Text.height 40) (identity title)) |> move (0, 80)]
                                else []

display : (Int,Int) -> GameModel.GameState -> Element
display (w,h) ({state,player,playerBullets,enemies,enemyBullets,ui,score} as gameState) =
    let displayedActors =
            [ rect (toFloat w) (toFloat h) |> filled black ] ++
            [ displayScore score ] ++
            displayStatus state ++
            displayActors enemies ++
            displayActors playerBullets ++
            displayActors enemyBullets ++
            --displayActors ui.lives ++
            [ displayActor player ]
    in container w h middle <| collage w h
        displayedActors