module Input where

import Keyboard

{-- Input ---------------------------------------------------------------------

------------------------------------------------------------------------------}

type Direction = { x:Int, y:Int }
type UserInput = { dir:Direction }

userInput : Signal UserInput
userInput =
    let dir = Keyboard.arrows
    in (UserInput <~ dir)

type Input = { timeDelta:Float, userInput:UserInput }