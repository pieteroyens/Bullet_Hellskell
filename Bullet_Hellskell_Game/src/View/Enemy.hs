-- Tells the game how to display an enemy.
module View.Enemy where

import Model.BoundingBox 
import Model.Enemy

import View.Renderable

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

instance Renderable Enemy where
    render e  = uncurry translate (enemyPos e) $ color (enemyColor $ enemyType e) $ shape e
    shape e   = rectangleSolid (width $ getBox e) (height $ getBox e)

-- Each enemy type should be displayed with a different color
enemyColor :: EnemyType -> Color
enemyColor eT
  | eT == Basic = makeColorI 255 116 0 255
  | otherwise   = makeColorI 255 30 0 255