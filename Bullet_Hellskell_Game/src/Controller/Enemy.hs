-- Module to control an enemy in the level.
module Controller.Enemy where

import Data.Maybe 

import Model.Ai
import Model.BoundingBox
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
import Controller.Player

import System.Random

-- update all the enemies' positions
moveEnemies :: Level -> Level
moveEnemies lvl = lvl { enemies = map move' (enemies lvl) }
  where 
    move' e 
      | ai e == None = move e (enemyVel e) 
      | otherwise    = chase e (player lvl) 

-- remove enemies that flew of the game game or that are pronouced dead.
removeEnemies :: Level -> Level
removeEnemies lvl = lvl { enemies = removeEntities $ map enemyFliesOfScreenSouth (enemies lvl) }

-- check if an enemy flies of the screen or not.
enemyFliesOfScreenSouth :: Enemy -> Enemy
enemyFliesOfScreenSouth e 
  | oos && (snd returnPosition /= snd (position e)) && snd (position e) < 0  = die e
  | otherwise                                                                = e
  where
      (oos, returnPosition) = outOfScreen e

-- let each enemy that can shoot spawn a bullet.
enemiesShoot :: Float -> Level -> Level
enemiesShoot elapsedTime lvl = lvl { enemies = updatedEnemies, bullets = bullets lvl ++ newBullets }
  where 
    (updatedEnemies, newBullets) = (map fst getEnemiesAndBullets, mapMaybe snd getEnemiesAndBullets)
    getEnemiesAndBullets         = map (enemyShoot elapsedTime) (enemies lvl)

-- let a single enemy should depending on the game time passed.
enemyShoot :: Float -> Enemy -> (Enemy, Maybe Bullet)
enemyShoot elapsedTime e 
  | elapsedTime >= enemyNextShot e && enemyType e == Shooting = (e { enemyNextShot = elapsedTime + enemyFireRate e }, Just (basicBullet bulletOrigin (0, -10) EnemyBullet ))
  | otherwise                                                 = (e, Nothing)
  where 
    bulletOrigin = position e - (0, 30)

-- check for each enemy whether it collides with the player
-- update the level accordingly
enemiesCollisionWithPlayer :: Float -> Level -> Level
enemiesCollisionWithPlayer elapsedTime lvl
  | length (enemies lvl) == length newEnemies = lvl
  | otherwise                                 = (deductLife elapsedTime lvl) { enemies = newEnemies, particles = particles lvl ++ newParticles }
  where
    (newEnemies, newParticles) = enemyCollisionWithPlayer (player lvl) (enemies lvl)

-- Check the player against every enemy if there is a collision
enemyCollisionWithPlayer :: Player -> [Enemy] -> ([Enemy], [Particle])
enemyCollisionWithPlayer p [] = ([], [])
enemyCollisionWithPlayer p es = (newEnemies, newCollisions)
  where
    newCollisions         = concatMap snd collisionCalculations
    newEnemies            = mapMaybe fst collisionCalculations
    collisionCalculations = map (doesEnemyCollidgeWithPlayer p) es

-- Check for a single enemy if it collides with the player
doesEnemyCollidgeWithPlayer :: Player -> Enemy -> (Maybe Enemy, [Particle])
doesEnemyCollidgeWithPlayer p e
    | collision p e && not (invulnerable p) = (Nothing, collapse (position e) (EnemyParticle (enemyType e)) 6)
    | otherwise                             = (Just e, [])

-- Adding enemies to the level
createWave :: GameState -> GameState
createWave gstate
  | null es   = createWave' waveDifficulty (gstate { gen = nextSeed, level = lvl' })
  | otherwise = gstate
  where
    lvl                        = level gstate
    lvl'                       = lvl { wave = currentWave + 1 }
    es                         = enemies lvl
    currentWave                = wave (level gstate)
    (waveDifficulty, nextSeed) = randomR (0, currentWave) (gen gstate)

-- create a wave of enemies depending on the difficulty.
createWave' :: Int -> GameState -> GameState
createWave' waveDiff gstate
  | waveDiff <= 1  = gstate { level = createEnemyLine Basic 1 (level gstate) }
  | waveDiff <= 2  = gstate { level = createEnemyLine Basic 2 (level gstate) }
  | waveDiff <= 3  = createRandomEnemies gstate
  | waveDiff <= 4  = gstate { level = createEnemySnake 14 (level gstate) }
  | waveDiff <= 5  = gstate { level = createEnemySnake 14 $ createEnemyLine Shooting 1 (level gstate) }
  | waveDiff <= 6 = gstate { level = createEnemySnake 14 $ createEnemyLine Shooting 2 (level gstate) }
  | otherwise      = gstate { level = createEnemySnake 14 $ createEnemyLine Shooting 2 (level gstate) }

-- create a straight line of enemies
createEnemyLine :: EnemyType -> Float -> Level -> Level
createEnemyLine eT howMany lvl = lvl { enemies = enemies lvl ++ line eT howMany }

-- formulates an enemy line
line :: EnemyType -> Float -> [Enemy]
line _  0       = []
line eT howMany = line eT (howMany - 1) ++ map (\x -> buildEnemy eT (startPosX + x, startPosY) None ) spacing
  where
    startPosX = -screenWidthHalf
    startPosY = screenHeightF + (50 * howMany)
    spacing   = [x * screenWidthF / 5 + (screenWidthF / 10) | x <- [0..4]]

-- create a straight vertical line of enemies with following behviour.
-- This creates a snake-like effect.
createEnemySnake :: Float -> Level -> Level
createEnemySnake number lvl = lvl { enemies = snake number ++ enemies lvl }

snake :: Float -> [Enemy]
snake ns = map (\n -> basicEnemy (0, screenHeightHalf + n * 40) Chase) [0..ns]

-- Just creates random enemies and a random amount of them.
createRandomEnemies :: GameState -> GameState
createRandomEnemies gstate = makeEnemy amount gstate'
  where 
    gstate'       = gstate { gen = rng }
    lvl           = level gstate
    (amount, rng) = randomR (0,10) (gen gstate)

-- Make an amount of random enemies
makeEnemy :: Int -> GameState -> GameState
makeEnemy 0      gstate = gstate
makeEnemy amount gstate = makeEnemy (amount - 1) gstate'
  where 
    gstate'                = gstate { gen = newSeed, level = lvl' } 
    lvl                    = level gstate
    lvl'                   = lvl { enemies = randomEnemy : enemies lvl }
    (randomEnemy, newSeed) = makeRandomEnemy (gen gstate)

-- Create a single random enemy with a random position, random enemy and 
-- ai type.
makeRandomEnemy :: StdGen -> (Enemy, StdGen)
makeRandomEnemy gen = (buildEnemy eType (xPos, yPos) aiType, seedD)
  where 
    (typeDecider, seedA) = randomR randomRange gen 
    (aiDecider,   seedB) = randomR randomRange seedA        
    (xPos,        seedC) = randomR (-screenWidthHalf,screenWidthHalf) seedB
    (yPos,        seedD) = randomR (screenHeightHalf,screenHeightHalf+50) seedC  
    eType                = if typeDecider == 0 then Basic else Shooting
    aiType               = if aiDecider   == 0 then None  else Chase 
    randomRange = (0, 1) :: (Int, Int)



