-- Data Module for that player. Holds all the usefull information
-- and data necessary for to do things with a player object.
module Model.Particle where

import Model.Moveable
import Model.Existance
import Model.Enemy
import Model.Bullet

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- Particles can be of three different types, this is mainly usefull for what the particle
-- behaviour should be and how it should be rendered.
data ParticleType = BulletParticle {bType :: BulletType} | EnemyParticle {eType :: EnemyType} | PlayerParticle deriving (Eq, Show)

-- Constructor for a particle. Different from other objects that can move, particle has acceleration.
data Particle = Particle { particlePos :: Point
                         , particleVel :: Vector
                         , particleAcc :: Vector
                         , particleType :: ParticleType
                         , timeAlive :: Float
                         , timeOfDeath :: Float
                         , particleLife :: Alive
                         } deriving (Eq, Show)

-- Use the acceleartion the particle has to move about the screen.
instance Moveable Particle where
    position   = particlePos
    move p vel = p { particlePos = position p + vel, particleVel = vel + particleAcc p }

instance Existance Particle where
    exist p                       = p { particleLife = Alive}
    die p                         = p { particleLife = Dead}
    isAlive p
        | particleLife p == Alive = True
        | otherwise               = False 