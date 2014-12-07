module GameInput where

import Keyboard

type Direction = { x:Int, y:Int }
type UserInput = { dir:Direction, rot:Int, fire1:Bool }

fireDelta = fps 3

userInput : Signal UserInput
userInput =
    let dir = Keyboard.arrows
        rot = lift .x Keyboard.wasd
        fire1 = Keyboard.space
    in (UserInput <~ dir
                   ~ rot
                   ~ fire1)

type Input = { timeDelta:Float, userInput:UserInput }