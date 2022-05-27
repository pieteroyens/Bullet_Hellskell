-- Controls the player in the game
module Controller.Player where

import Model.Bullet 
import Model.Existance  
import Model.Level
import Model.Model
import Model.Moveable
import Model.Particle
import Model.Player
import Model.Screen

import Controller.Collision
import Controller.Particle
    
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

-- Check if the player is still invulnerable, if not, set the player to vulnerable so
-- the player can be hit again.
checkInvulnerable :: Float -> Level -> Level
checkInvulnerable elapsedTime lvl
  | invulnerableUpTo (player lvl) < elapsedTime = lvl { player = (player lvl) { invulnerable = False }}
  | otherwise                                   = lvl

-- move the player around with the w,a,s,d keys.
movePlayer :: Level -> KeyStates -> Level
movePlayer lvl ks 
  | wKey ks == Down   = lvl { player = checkPlayerBounds $ move (player lvl) (0, 10) }
  | aKey ks == Down   = lvl { player = checkPlayerBounds $ move (player lvl) (-10, 0) }
  | sKey ks == Down   = lvl { player = checkPlayerBounds $ move (player lvl) (0, -10) }
  | dKey ks == Down   = lvl { player = checkPlayerBounds $ move (player lvl) (10, 0) }
  | otherwise         = lvl { player = checkPlayerBounds $ move (player lvl) (0, 0) }

-- Check if the player does not go out of screen bounds, if so, put the player
-- back just on the bord of the screen.
checkPlayerBounds :: Player -> Player
checkPlayerBounds p 
    | oos = p { playerPos = returnPosition }
    | otherwise   = p
    where
        (oos, returnPosition) = outOfScreen p

-- remove a life from the player. If the lives reach 0, the game is over and the level status is changed.
deductLife :: Float -> Level -> Level
deductLife elapsedTime lvl 
  | invulnerable (player lvl)                        = lvl
  | newLives <= 0 && not (invulnerable (player lvl)) = lvl { player = deductLife' elapsedTime (player lvl), levelStatus = GameOver }
  | otherwise                                        = lvl { player = deductLife' elapsedTime (player lvl) }
  where
    newLives = lives (deductLife' elapsedTime (player lvl))

-- when a life is removed, the player turn invulnerable for three seconds. 
deductLife' :: Float -> Player -> Player
deductLife' elapsedTime p = p { lives = lives p - 1, invulnerableUpTo = elapsedTime + 3.0, invulnerable = True }

-- Check if the player is hit by any of the enemy bullets. If so, deduct a life.
playerBulletCollision :: Float -> Level -> Level 
playerBulletCollision elapsedTime lvl 
  | collisionOccured = deductLife elapsedTime $ lvl { bullets = updatedBullets, particles = particles lvl ++ updatedParticles  } 
  | otherwise        = lvl
  where
    collisionOccured = length updatedBullets /= length (bullets lvl)
    updatedBullets   = fst $ playerCollidesWithBullets (player lvl) (bullets lvl)
    updatedParticles = snd $ playerCollidesWithBullets (player lvl) (bullets lvl)

-- If the player is hit by any of the enemy bullets, remove the bullet from the game and
-- generate a particle explosion.
playerCollidesWithBullets :: Player -> [Bullet] -> ([Bullet], [Particle])
playerCollidesWithBullets p = foldr (\b (bss, pss) -> if trueCollision b 
                                                         then (bss, pss ++ explosion b) 
                                                         else (b : bss, pss) ) ([], [])
  where
    trueCollision = collision p
    explosion b   = if trueCollision b 
                       then createExplosion' (position b) (BulletParticle (bulletType b)) 3 
                       else []

-- How the player should shoot bullets. The player can should when the delay between
-- shots if over and if the up-key is pressed down.
playerShoot :: Float -> Level -> KeyStates -> Level 
playerShoot elapsedTime lvl ks    
  | kUp ks == Down && canShoot = lvl { bullets = basicBullet bulletOrigin (0, 20) PlayerBullet : bullets lvl, player = (player lvl) { nextFire = elapsedTime + fireRate (player lvl)  } }
  | otherwise                  = lvl
  where
    canShoot     = elapsedTime >= nextFire (player lvl)
    bulletOrigin = position (player lvl) + (0, 10)