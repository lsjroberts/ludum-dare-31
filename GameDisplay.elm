module GameDisplay where

import Debug

import GameModel as GameModel
import GameEnemies as GameEnemies

displayActor ({spr,pos,rot} as actor) =
    spr.shape |> move (pos.x, pos.y)
              |> rotate(radians rot.angle)
              |> rotate(pi/2)

displayActors : [GameModel.Actor] -> [Form]
displayActors actors =
    actors |> map (\actor -> displayActor actor)

--displayEnemyGroups : [GameModel.EnemyGroup] -> [Form]
--displayEnemyGroups groups =
--    groups |> map (\group -> .enemies group)
--           |> displayActors

display : (Int,Int) -> GameModel.GameState -> Element
display (w,h) ({player,playerBullets,enemies} as gameState) =
    let displayedActors = --displayEnemyGroups enemies
                        displayActors playerBullets
                       ++ [ displayActor player ]
    in container w h middle <| collage w h
        displayedActors