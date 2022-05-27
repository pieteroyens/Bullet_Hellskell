-- Data Module for that enemy. Holds all the usefull information
-- and data necessary for to do things with an enemy object.
module Model.Enemy where

import Model.Ai
import Model.BoundingBox    
import Model.Existance
import Model.Moveable

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- Defines the two enemy types in this game.
data EnemyType = Basic | Shooting deriving (Eq, Show)

-- All information needed to reason about an enemy object in the game.
data Enemy = Enemy { enemyPos :: Point
                   , enemyVel :: Vector
                   , box :: Box
                   , enemyLife :: Alive
                   , enemyType :: EnemyType
                   , enemyNextShot :: Float
                   , enemyFireRate :: Float
                   , ai :: AiType } deriving (Show)

instance Moveable Enemy where
    position   = enemyPos
    move e vel = e { enemyPos = enemyPos e + vel }

instance BoundingBox Enemy where
    getBox = box

instance Ai Enemy where
  chase enemy target = move enemy (fDirX * 6, snd (enemyVel enemy))
    where
     (eX, eY)         = position enemy
     (tX, tY)         = position target
     dirX             = tX - eX
     dirY             = tY - eY
     magnitude        = magV (dirX, dirY)
     (fDirX, fDirY)   = (dirX/magnitude, dirY/magnitude)
        

instance Existance Enemy where
    exist e = e { enemyLife = Alive }
    die e   = e { enemyLife = Dead}
    isAlive e
        | enemyLife e == Alive = True
        | otherwise            = False

-- Build an enemy defined on type an Ai, a starting point for the enemy
-- must also been given.
buildEnemy :: EnemyType -> Point -> AiType -> Enemy
buildEnemy Basic    pos aiType = Enemy pos (0, -3) (RectangleBox 20 20) Alive Basic 0 0 aiType
buildEnemy Shooting pos aiType = Enemy pos (0, -3) (RectangleBox 30 30) Alive Shooting 0.5 2 aiType

-- gives back a basic enemy that just goes straight down. You can add behviour to it as well.
basicEnemy :: Point -> AiType -> Enemy
basicEnemy pos aiType = Enemy pos (0, -3) (RectangleBox 20 20) Alive Basic 0 0 aiType

-- gives back a basic enemy that just shoots and goes straight down. You can add behviour to it as well.
shootingEnemy :: Point -> AiType -> Enemy
shootingEnemy pos aiType = Enemy pos (0, -3) (RectangleBox 30 30) Alive Shooting 0.5 2 aiType