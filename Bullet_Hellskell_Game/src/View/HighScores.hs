-- Logic to show a list of scores.
module View.HighScores where

import Graphics.Gloss

-- Transform the scores list into one single displable picture
showScores :: String -> Picture
showScores scores = pictures $ showScores' (lines scores) 1

-- Put a list of scores nicely in a line so the user can view it easily.
showScores' :: [String] -> Int -> [Picture]
showScores' []     _ = []
showScores' (s:ss) n
    | n < 10    = number : score : showScores' ss (n + 1)
    | otherwise = number : score : showScores' ss (n + 1)

    where 
      number = translate (-100) (250 - fromIntegral n * 50) $ scale 0.3 0.3 $ color white $ text (show n ++ ".")
      score  = translate 70 (250 - fromIntegral n * 50) $ scale 0.3 0.3 $ color white $ text s
