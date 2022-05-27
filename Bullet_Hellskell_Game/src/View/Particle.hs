-- Defined how a particular particle should be shown on the screen.
module View.Particle where

import Model.Enemy    
import Model.Moveable
import Model.Particle 

import View.Bullet
import View.Enemy
import View.Player
import View.Renderable

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

instance Renderable Particle where
    render p = uncurry translate (position p) $ color (particleColor' (particleType p)) $ shape p
    shape p = shape' $ particleType p

-- Which geometric shape a particle should have depends on the type of particle.    
shape' :: ParticleType -> Picture
shape' BulletParticle {}  = circleSolid 2.0
shape' EnemyParticle {} = rectangleSolid 7.0 7.0
shape' PlayerParticle   = rectangleSolid 7.0 7.0

-- Which color a particle should have depends on the type of particle.
-- In case the particle is spawned from a bullet, it should have the same color
-- as the bullet, or if it is spawned from an enemy, should have the same color
-- as the enemy.    
particleColor' :: ParticleType -> Color
particleColor' BulletParticle {bType = bT } = bulletColor bT
particleColor' EnemyParticle {eType = eT }  = enemyColor eT
particleColor' PlayerParticle               = blue