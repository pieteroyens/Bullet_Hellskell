-- Module about particle effects in the game
module Controller.Particle where

import Model.Existance  
import Model.Level
import Model.Model
import Model.Moveable
import Model.Particle

import Controller.Existance

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- moves each particle around the screen
moveParticles :: Level -> Level
moveParticles lvl    = lvl { particles = map move' currentParticles }
  where 
    currentParticles = particles lvl
    move' p          = move p (particleVel p) 

-- Particle existance is based on a time limit. If the time limit is up, the
-- particle should be removed from the level.
updateParticlesLife :: Float -> Level -> Level
updateParticlesLife elapsedTime lvl    = lvl { particles = particles' }
  where
    particles'                         = removeEntities $ map checkLifeLimit (particles lvl)
    checkLifeLimit part
      | timeOfDeath part == 0          = part { timeOfDeath = elapsedTime + timeAlive part }
      | elapsedTime > timeOfDeath part = die part
      | otherwise                      = part

-- Collapsing particle effect. Right now it just looks like a quarter of an explosion though...
collapse :: Point -> ParticleType -> Float -> [Particle]
collapse origin pT total = map (collapseParticle origin 10 0.5 pT total) [0..total]

collapseParticle :: Point -> Float -> Float -> ParticleType -> Float -> Float -> Particle
collapseParticle origin velSpeed accSpeed partType total current = Particle origin vel acc partType 1.5 0 Alive
  where 
    (xDir, yDir) = collapseDirection total current
    vel          = (velSpeed * xDir, velSpeed * yDir)
    acc          = (accSpeed * xDir, accSpeed * yDir)

collapseDirection :: Float -> Float -> (Float, Float)
collapseDirection total current = (cos step, sin step )
    where 
        step = (7 * pi / 6) + ((4 * pi / 6) / total * current)
     
-- Create an explosition of particles. 
createExplosion :: Point -> ParticleType -> Float -> Level -> Level
createExplosion origin partType howMany lvl = lvl { particles = particles lvl ++ createExplosion' origin partType howMany }-- createExplosion' origin partType howMany howMany [] }

-- Helper explosion function
createExplosion' :: Point -> ParticleType -> Float -> [Particle]
createExplosion' origin pT total = map (explosionParticle origin velSpeed accSpeed pT total) [1..total]
  where 
    velSpeed = fst $ explosionSpeeds pT
    accSpeed = snd $ explosionSpeeds pT
  
-- how fast something explodes depends on the particle type
explosionSpeeds :: ParticleType -> (Float, Float)
explosionSpeeds BulletParticle {} = (5, -0.8)
explosionSpeeds PlayerParticle    = (4, 0.05)
explosionSpeeds EnemyParticle {}  = (3, -0.05)

-- Gives a single particle in the explosion.
explosionParticle :: Point -> Float -> Float -> ParticleType -> Float -> Float -> Particle
explosionParticle origin velSpeed accSpeed partType total left = Particle origin vel acc partType 1.5 0 Alive
  where 
    (xDir, yDir) = explosionXYDirection total left
    vel          = (velSpeed * xDir, velSpeed * yDir)
    acc          = (accSpeed * xDir, accSpeed * yDir)

-- Particles' direction is based on the unit circle.
explosionXYDirection :: Float -> Float -> (Float, Float)
explosionXYDirection total left = (cos (2 * pi / total * left), sin (2 * pi / total * left))




        