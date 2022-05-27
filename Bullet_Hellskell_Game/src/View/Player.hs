-- Player View, define how a player should be rendered to the screen.
module View.Player where

import Model.BoundingBox    
import Model.Player

import View.Renderable

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

instance Renderable Player where
    render p      = uncurry translate (playerPos p) $ color (playerColor' p) $ shape p
    shape p       = rectangleSolid (width $ getBox p) (height $ getBox p)

-- If the player is invulenerable / hit by an enemy, turn the color white
playerColor' :: Player -> Color
playerColor' p
    | invulnerable p = white
    | otherwise      = playerColor

playerColor :: Color
playerColor = makeColorI 3 129 183 255