module Main where

import Debug
import Window

import GameInput as GameInput
import GameModel as GameModel
import GameUpdates as GameUpdates
import GameDisplay as GameDisplay


delta = inSeconds <~ fps 30
input = Debug.watch "input" <~ sampleOn delta (lift2 GameInput.Input delta GameInput.userInput)

gameState = foldp GameUpdates.stepGame GameModel.defaultGame input

main = lift2 GameDisplay.display Window.dimensions gameState
