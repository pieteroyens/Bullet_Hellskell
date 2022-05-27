-- Defines a types class for everythin that can move around on the screen.
module Model.Moveable where

import Graphics.Gloss

class Moveable a where
    position :: a -> Point
    move     :: a -> Vector -> a