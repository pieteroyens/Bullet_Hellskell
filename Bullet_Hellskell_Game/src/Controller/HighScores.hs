-- Defined how the scores should be written into a new string list.
-- this list is then again written back into the file.
module Controller.HighScores where

updateScores :: Int -> String -> String
updateScores currentScore = unlines . map show . updateScores' currentScore . map read . lines

updateScores' :: Int -> [Int] -> [Int]
updateScores' _            []  = []
updateScores' currentScore [s]
  | currentScore > s           = [currentScore]
  | otherwise                  = [s]
updateScores' currentScore (s:ss)
  | currentScore > s           = currentScore : s : init ss
  | otherwise                  = s : updateScores' currentScore ss