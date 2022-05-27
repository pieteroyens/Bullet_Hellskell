-- Defines level data constructor.
module Model.Level where

import Model.Bullet
import Model.Enemy
import Model.Particle
import Model.Player

-- pretty self explanatory, comes in handy when deciding if the 
-- game needs to be updated.
data LevelStatus = Playing | Paused | GameOver deriving (Eq, Show)

data Level = Level { player  :: Player
                   , enemies :: [Enemy]
                   , bullets  :: [Bullet]
                   , particles :: [Particle]
                   , levelStatus :: LevelStatus
                   , score :: Int
                   , wave :: Int
                   } deriving (Show)
