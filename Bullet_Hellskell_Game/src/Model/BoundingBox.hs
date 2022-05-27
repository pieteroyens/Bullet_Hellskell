-- Defined a type class for things that have a bounding box around them.
-- Since the game only uses geometric shapes, the boundingbox is also usefull 
-- to help draw an object depending on its bounding box.
module Model.BoundingBox where

data Box = CircleBox { radius :: Float } | RectangleBox { width :: Float, height :: Float } deriving (Eq, Show)

class BoundingBox a where 
    getBox :: a -> Box
    