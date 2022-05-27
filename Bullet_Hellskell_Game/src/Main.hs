module Main where

import System.Random
        
import Controller.Controller

import Model.Model      
import Model.Screen

import View.View     

import Graphics.Gloss.Interface.IO.Game

-- main loop of the game
main :: IO ()
main = do gen <- newStdGen
          highScores <- readFile "highscores.txt"
          playIO (InWindow "Bullet = Spawn Bullet" (screenWidth, screenHeight) (0, 0))
                  black            
                  30           
                  (initialState gen highScores) 
                  view             
                  input    
                  update           