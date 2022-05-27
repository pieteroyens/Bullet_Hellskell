-- Main game controller
module Controller.Controller where

import System.Random
import System.IO
import System.Directory 

import Data.Maybe
import Data.List

import Model.Model
import Model.Level

import Controller.Bullet
import Controller.Enemy
import Controller.HighScores
import Controller.Player
import Controller.Particle

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Handle one iteration of the game/ Update the game
update :: Float -> GameState -> IO GameState
update secs gstate = if writeCheck gstate == Write -- check if we have to write to the file system, if so, write once.
                        then 
                          do (tempName, tempHandle) <- openTempFile "." "temp"
                             let newScores = updateScores (score (level gstate)) (highScores gstate)
                             hPutStr tempHandle newScores
                             hClose tempHandle
                             removeFile "highScores.txt"
                             renameFile tempName"highScores.txt"
                             return $ updateGame secs ( gstate { highScores = newScores, writeCheck = DontWrite } )
                        else 
                          return $ updateGame secs gstate
                                                  
                
-- update the game depending on what the level status is.
updateGame :: Float -> GameState -> GameState
updateGame secs gstate 
  | levelStatus (level gstate) == GameOver && 
    writeCheck gstate == HaveToWrite          = gstate { whatToShow = ShowGameOver, writeCheck = Write }
  | levelStatus (level gstate) == GameOver    = gstate { whatToShow = ShowGameOver }
  | levelStatus (level gstate) == Paused      = gstate 
  | levelStatus (level gstate) == Playing     = generateEnemies $ updateParticles $ updateEnemies $ updateBullets $ updatePlayer $ updateElapsedTime secs gstate
  | otherwise                                 = gstate

-- Update all the ingame object like the player, gametime, bullets, enemies and particles. Also generates enemies
-- and update user input. 

updateElapsedTime :: Float -> GameState -> GameState
updateElapsedTime secs gstate = gstate { elapsedTime = elapsedTime gstate + secs }

updatePlayer :: GameState -> GameState
updatePlayer gstate = gstate { level = playerBulletCollision (elapsedTime gstate) $ checkInvulnerable (elapsedTime gstate) $ movePlayer (level gstate) (keyStates gstate) }

updateBullets :: GameState -> GameState
updateBullets gstate = gstate { level = removeBullets $ bulletsCollide $ updateLiveBullets $ moveBullets (elapsedTime gstate) $ playerShoot (elapsedTime gstate) (level gstate) (keyStates gstate)  }

updateEnemies :: GameState -> GameState
updateEnemies gstate = gstate { level = removeEnemies $ enemiesCollisionWithPlayer (elapsedTime gstate) $ moveEnemies $ enemiesShoot (elapsedTime gstate) (level gstate) }

generateEnemies :: GameState -> GameState
generateEnemies = createWave

updateParticles :: GameState -> GameState
updateParticles gstate = gstate { level = updateParticlesLife (elapsedTime gstate) $ moveParticles (level gstate)}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
  | c == 'w'                                        = gstate { keyStates = (keyStates gstate) { wKey = Down} }
  | c == 'a'                                        = gstate { keyStates = (keyStates gstate) { aKey = Down} }
  | c == 's'                                        = gstate { keyStates = (keyStates gstate) { sKey = Down} }
  | c == 'd'                                        = gstate { keyStates = (keyStates gstate) { dKey = Down} }
  | c == 'p' && (whatToShow gstate /= ShowGameOver) = if whatToShow gstate == ShowPauseMenu || whatToShow gstate == ShowHighScores 
                                                         then gstate { whatToShow = ShowLevel, level = (level gstate) { levelStatus = Playing } }
                                                         else gstate { whatToShow = ShowPauseMenu, level = (level gstate) { levelStatus = Paused } }
  | c == 'h' && (whatToShow gstate /= ShowGameOver) = if whatToShow gstate == ShowLevel || whatToShow gstate == ShowPauseMenu 
                                                         then gstate { whatToShow = ShowHighScores, level = (level gstate) { levelStatus = Paused } }
                                                         else gstate { whatToShow = ShowPauseMenu }
  | c == 'r'                                        = if levelStatus (level gstate) == GameOver && whatToShow gstate == ShowGameOver 
                                                         then initialState (gen gstate) (highScores gstate) 
                                                         else gstate
  | otherwise                                       = gstate
inputKey (EventKey (Char c) Up _ _) gstate
  | c == 'w'                                        = gstate { keyStates = (keyStates gstate) { wKey = Up} }
  | c == 'a'                                        = gstate { keyStates = (keyStates gstate) { aKey = Up} }
  | c == 's'                                        = gstate { keyStates = (keyStates gstate) { sKey = Up} }
  | c == 'd'                                        = gstate { keyStates = (keyStates gstate) { dKey = Up} }
  | otherwise                                       = gstate
inputKey (EventKey (SpecialKey sk) Down _ _) gstate
  | sk == KeyUp                                     = gstate { keyStates = (keyStates gstate) { kUp = Down} }
inputKey (EventKey (SpecialKey sk) Up _ _) gstate
  | sk == KeyUp                                     = gstate { keyStates = (keyStates gstate) { kUp = Up} }
inputKey _ gstate = gstate