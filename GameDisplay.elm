module GameDisplay where

import GameModel as GameModel

display : (Int,Int) -> GameModel.GameState -> Element
display (w,h) ({player} as gameState) =
    container w h middle <| collage GameModel.gameWidth GameModel.gameHeight
        --(Draw.display [ player.spr.shape ])
        [ player.spr.shape |> move (player.pos.x, player.pos.y) |> rotate(radians player.rot.angle) ]