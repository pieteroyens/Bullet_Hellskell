-- | This module contains the data types
--   which represent the state of the game
module Model.Model where

import Model.Ai   
import Model.BoundingBox
import Model.Bullet  
import Model.Enemy
import Model.Existance
import Model.Level
import Model.Moveable
import Model.Particle
import Model.Player



-- Basic models for all the keys and the game state in general.

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Different kinds of keystates used to interact with the game. 
data KeyStates = KeyStates { wKey :: KeyState
                           , aKey :: KeyState
                           , sKey :: KeyState
                           , dKey :: KeyState
                           , kUp :: KeyState
                           , mouseL :: KeyState
                           }

-- Usefull information for the view on what to render.
data WhatToShow = ShowLevel | ShowHighScores | ShowPauseMenu | ShowGameOver deriving (Eq, Show)

-- Helper data to check wether or not to write to the file system/ write
-- highscores
data WriteCheck = Write | HaveToWrite | DontWrite deriving (Eq, Show)

-- GameState hold all data needed for a gamestate. 
data GameState =  GameState { gen :: StdGen
                            , highScores :: String
                            , writeCheck :: WriteCheck
                            , whatToShow :: WhatToShow
                            , level :: Level
                            , keyStates :: KeyStates
                            , elapsedTime :: Float
                            }

initialLevel :: Level
initialLevel = Level player1 [] [] [] Playing 0 0

initialKeyStates :: KeyStates
initialKeyStates = KeyStates Up Up Up Up Up Up

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.1

initialState :: StdGen -> String -> GameState
initialState rng hS = GameState rng hS HaveToWrite ShowLevel initialLevel initialKeyStates 0