-- Bullet View, define how a bullet should be rendered to the screen.
module View.Bullet where

import Model.Bullet
import Model.BoundingBox
import View.Renderable

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

instance Renderable Bullet where
    render b      = uncurry translate (bulletPos b) $ color (bulletColor (bulletType b)) $ shape b
    shape b       = circleSolid (radius $ getBox b)

-- Each bullet should have a different type depending on who shot it
-- i.e. depending on type.
bulletColor :: BulletType -> Color
bulletColor PlayerBullet = light blue
bulletColor EnemyBullet  = dark red