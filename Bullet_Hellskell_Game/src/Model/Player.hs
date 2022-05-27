-- Data Module for that player. Holds all the usefull information
-- and data necessary for to do things with a player object.
module Model.Player where

import Model.Moveable
import Model.Existance
import Model.BoundingBox

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- Data constructor for the player, construcotor elements speak for themselves.
data Player = Player { playerPos :: Point
                     , playerVel :: Vector
                     , playerBox :: Box 
                     , fireRate :: Float
                     , nextFire :: Float
                     , lives :: Int 
                     , alive :: Alive
                     , invulnerableUpTo :: Float
                     , invulnerable :: Bool
                     } deriving (Show)

instance Moveable Player where
    position   = playerPos
    move p vel = p { playerPos = playerPos p + vel }

instance BoundingBox Player where
    getBox = playerBox

-- a constant to define a basic player object. This is used to initialize the player.
player1 :: Player
player1 = Player (0,-200) (0,0) (RectangleBox 30 30) 0.5 0 3 Alive 0 False