-- Defines types class for things that can have "smart behaviour".
module Model.Ai where

import Model.Moveable

import Graphics.Gloss.Data.Vector

data AiType = None | Chase deriving (Eq, Show)

class Moveable a => Ai a where
    -- When implemented should be a function that would imitate chasing the player
    chase :: Moveable b => a -> b -> a 
