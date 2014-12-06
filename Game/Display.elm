module Display where

{-- Display -------------------------------------------------------------------

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) ({player} as gameState) =
    container w h middle <| collage gameWidth gameHeight
        --(Draw.display [ player.spr.shape ])
        [ move (player.pos.x, player.pos.y) player.spr.shape ]