-- Simple constants about the game screen, usefull form some game logic.
module Model.Screen where
    
screenWidth :: Int
screenWidth = 500

screenHeight :: Int
screenHeight = 700

screenWidthF :: Float
screenWidthF = fromIntegral screenWidth

screenHeightF :: Float
screenHeightF = fromIntegral screenHeight

screenHeightHalf :: Float
screenHeightHalf = screenHeightF / 2

screenWidthHalf :: Float
screenWidthHalf = screenWidthF / 2