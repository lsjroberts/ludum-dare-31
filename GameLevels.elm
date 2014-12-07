module GameLevels where

import Random

levelsSeed : String
levelsSeed = "foo" -- generate once and save?

levelOne : Level
levelOne = { name = "One"
           , difficulty = 1
           , seed = levelsSeed }

levels : Levels
levels = [ levelOne ]