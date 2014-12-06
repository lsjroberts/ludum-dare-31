module Main where

import Debug
import Keyboard
import Window

import Game.Input as GameInput
import Game.Model as GameModel
import Game.Updates as GameUpdates
import Game.Display as GameDisplay

delta = inSeconds <~ fps 30
input = Debug.watch "input" <~ sampleOn delta (lift2 Input delta GameInput.userInput)

gameState = foldp GameUpdates.stepGame GameModel.defaultGame GameModel.input

main = lift2 GameDisplay.display Window.dimensions gameState
