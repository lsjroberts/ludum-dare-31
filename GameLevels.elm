module GameLevels where

import Random

type Level = { name:String, difficulty:Int, seed:String }
type Levels = [Level]

levelsSeed : String
levelsSeed = "foo" -- generate once and save?

levelOne : Level
levelOne = { name = "One"
           , difficulty = 1
           , seed = levelsSeed }

levels : Levels
levels = [ levelOne ]