module Main where

import Window

{-- Input ---------------------------------------------------------------------

------------------------------------------------------------------------------}

type UserInput = {}

userInput : Signal UserInput
userInput = constant {}

type Input = { timeDelta:Float, userInput:UserInput }



{-- Model ---------------------------------------------------------------------

------------------------------------------------------------------------------}

type GameState = {}

defaultGame : GameState
defaultGame = {}



{-- Updates -------------------------------------------------------------------

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = gameState



{-- Display -------------------------------------------------------------------

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = asText gameState



{-- And put it together -------------------------------------------------------

------------------------------------------------------------------------------}

delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
