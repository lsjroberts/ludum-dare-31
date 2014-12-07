module GameInput where

import Keyboard

type Direction = { x:Int, y:Int }
type UserInput = { dir:Direction, fire1:Bool }

fireDelta = fps 3

userInput : Signal UserInput
userInput =
    let dir = merge Keyboard.arrows Keyboard.wasd
        fire1 = Keyboard.space
    in (UserInput <~ dir
                   ~ fire1)

type Input = { timeDelta:Float, userInput:UserInput }