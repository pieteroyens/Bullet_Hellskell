-- Defines a typeclass for things that can dissapear
-- i.e. "die" and should be removed from the game.
module Model.Existance where

data Alive = Alive | Dead deriving (Eq, Show)

class Existance a where
    exist   :: a -> a
    die     :: a -> a
    isAlive :: a -> Bool