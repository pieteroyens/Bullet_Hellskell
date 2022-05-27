-- Data Module for a bullet. Holds all the usefull information
-- and data necessary for to do things with a bullet object.
module Model.Bullet where

import Model.BoundingBox    
import Model.Enemy
import Model.Existance
import Model.Moveable 

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- different kinds of bullets. Helpful when checking collision an how to
-- render a bullet.
data BulletType = EnemyBullet | PlayerBullet deriving (Eq, Show)

-- Data needed to reason and manipulate a bullet in the game
data Bullet = Bullet { bulletPos :: Point
                     , bulletVel :: Vector
                     , bulletType :: BulletType
                     , bulletBox :: Box
                     , bulletLife :: Alive } deriving (Eq, Show)

instance Moveable Bullet where
    position   = bulletPos
    move b vel = b { bulletPos = position b + vel }

instance BoundingBox Bullet where
    getBox = bulletBox

instance Existance Bullet where
  exist b                  = b { bulletLife = Alive }
  die b                    = b { bulletLife = Dead}
  isAlive b 
   | bulletLife b == Alive = True
   | otherwise             = False 

-- Just a basic bullet, give a starting position and how fast you want it to go.
-- Also give the type, i.e. who shot it should be the type.
basicBullet :: Point -> Vector -> BulletType -> Bullet
basicBullet pos vel bt = Bullet pos vel bt (CircleBox 5) Alive