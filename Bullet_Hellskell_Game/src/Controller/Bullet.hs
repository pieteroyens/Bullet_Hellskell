-- Module that controls bullets in the game.
module Controller.Bullet where

import Data.List

import Model.Bullet
import Model.Enemy
import Model.Existance
import Model.Level
import Model.Model
import Model.Moveable
import Model.Particle
import Model.Player
import Model.Screen

import Controller.Collision
import Controller.Existance
import Controller.Particle

-- Update bullet positions
moveBullets :: Float -> Level -> Level
moveBullets elapsedTime lvl = lvl { bullets = allBullets }
  where 
   currentBullets = bullets lvl
   allBullets     = map (\b -> move b (bulletVel b)) currentBullets

-- remove bullets when they are dead
removeBullets :: Level -> Level
removeBullets lvl = lvl { bullets = removeEntities (bullets lvl) }

-- update Live status when the fly off the screen to not alive anymore
updateLiveBullets :: Level -> Level
updateLiveBullets lvl = lvl { bullets = map bulletWithinBounds (bullets lvl) } 

bulletWithinBounds :: Bullet -> Bullet
bulletWithinBounds b 
  | fst $ outOfScreen b = die b
  | otherwise           = b

-- Check for all the bullets in the level if they collide with the enemies. 
-- Update the level accordingly.
bulletsCollide :: Level -> Level
bulletsCollide lvl = lvl { bullets = bulletsLeft, enemies = enemiesLeft, score = score lvl + (length (enemies lvl) - length enemiesLeft), particles = particles lvl ++ explosions }
  where 
    (bulletsLeft, enemiesLeft, explosions) = getBulletsEnemiesAndExplosions (bullets lvl) (enemies lvl)
  
-- When a bullet hits an enemy, remove the enemy and bullet from the lists and generate two explosions.
getBulletsEnemiesAndExplosions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Particle])
getBulletsEnemiesAndExplosions bs = detectCollision bs []
  where
    detectCollision :: [Bullet] -> [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Particle])
    detectCollision [] fb [] = (fb,[],[])
    detectCollision [] fb es = (fb, es, [])
    detectCollision bs fb [] = (bs ++ fb, [], [])
    detectCollision (b:bs) fb es 
      | fst $ getBulletEnemyAndExplosions b es = (bs ++ fb, restOfTheEnemies b es , snd $ getBulletEnemyAndExplosions b es)
      | otherwise                               = detectCollision bs (b:fb) es

    getBulletEnemyAndExplosions :: Bullet -> [Enemy] -> (Bool, [Particle])
    getBulletEnemyAndExplosions b es
      | length (checkEveryEnemy b es) == 1 = (True, snd $ head $ checkEveryEnemy b es)
      | otherwise                            = (False, [])

    restOfTheEnemies b   = filter (not . fst . bulletEnemyCollision b) 
    checkEveryEnemy b es = filter fst $ map (bulletEnemyCollision b) es
  
-- Single check per bullet and enemy. If there is a collision, return a True and two explosions.
bulletEnemyCollision :: Bullet -> Enemy -> (Bool, [Particle])
bulletEnemyCollision b e 
  | collision b e && bulletType b == PlayerBullet = (True, bulletExplosion ++ enemyExplosion)
  | otherwise                                     = (False, [])
  where
    bulletExplosion = createExplosion' (position b) (BulletParticle (bulletType b)) 10
    enemyExplosion  = createExplosion' (position e) (EnemyParticle (enemyType e)) 5