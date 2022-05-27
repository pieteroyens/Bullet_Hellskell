-- Main View module. Turns the game into a viewable picture for the user.
module View.View where

import Model.Model
import Model.Level
import Model.Player
import View.Renderable
import View.HighScores

import View.Player
import View.Bullet
import View.Enemy
import View.Particle

import Graphics.Gloss

view :: GameState -> IO Picture
view gstate = return $ viewPure gstate

-- Depending on what the game state wants to show, render it to the screen
viewPure :: GameState -> Picture
viewPure gstate
  | whatToShow gstate == ShowPauseMenu  = translate (-200) 0 $ color white $ text "paused"
  | whatToShow gstate == ShowHighScores = pictures [hsTitle,  highScs]
  | whatToShow gstate == ShowGameOver   = pictures [sc, gameOver]
  | otherwise                           = pictures [l, p, bs, es, sc, ps, w]
    where
      sc       = translate 100 300 $ scale 0.2 0.2 $ color white $ text $ "score: " ++ show (score (level gstate)) -- render the score
      p        = render (player (level gstate))                                                                    -- render the player
      bs       = pictures $ map render (bullets (level gstate))                                                    -- render the bullets
      es       = pictures $ map render (enemies (level gstate))                                                    -- render the enemies
      ps       = pictures $ map render (particles (level gstate))                                                  -- render the particles
      l        = translate (-220) 300 $ scale 0.2 0.2 $ color white $ text $ "lives: " ++ show (lives (player (level gstate))) -- render lives
      w        = translate 130 (-300) $ scale 0.2 0.2 $ color white $ text $ "wave: " ++ show (wave (level gstate)) -- render the waves
      gameOver = translate (-200) 0 $ scale 0.5 0.5 $ color red $ text "Game Over"                                  -- render the gameOver Text
      hsTitle  = translate (-100) 300 $ scale 0.3 0.3 $ color white $ text "High Scores"                            -- render the highscores text
      highScs  = showScores (highScores gstate)                                                                     -- render the actual scores