-- Module to handle object collision in game, only export the collision function and outOFscreen Function
module Controller.Collision (collision, outOfScreen) where

import Model.BoundingBox
import Model.Moveable
import Model.Screen

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- Check geomtric collision based on position and boundingBox of an object.
collision :: (BoundingBox a, Moveable a, BoundingBox b, Moveable b) => a -> b -> Bool
collision objectA objectB = collision' (getBox objectA) (position objectA) (getBox objectB) (position objectB)

collision' :: Box -> Point -> Box -> Point -> Bool
collision' (CircleBox r1)       p1 (CircleBox r2)       p2 = True -- Not Implemented, was not needed
collision' (CircleBox r1)       p1 (RectangleBox w2 h2) p2 = circleRectangleCollion r1 p1 w2 h2 p2
collision' (RectangleBox w1 h1) p1 (CircleBox r2)       p2 = circleRectangleCollion r2 p2 w1 h1 p1
collision' (RectangleBox w1 h1) p1 (RectangleBox w2 h2) p2 = rectRectCollion w1 h1 p1 w2 h2 p2


circlePointCollion :: Float -> Point -> Bool
circlePointCollion cRadius (pX, pY) = (pX * pX + pY * pY) < (cRadius * cRadius)

circleRectangleCollion :: Float -> Point -> Float -> Float -> Point -> Bool
circleRectangleCollion cRadius (cx, cy) rw rh (rx, ry) = circlePointCollion cRadius (deltaX, deltaY)
  where
    nearestX = max (rx - rw/2) (min cx (rx + rw/2))
    nearestY = max (ry - rh/2) (min cy (ry + rh/2))
    deltaX   = cx - nearestX
    deltaY   = cy - nearestY

rectRectCollion :: Float -> Float -> Point -> Float -> Float -> Point -> Bool
rectRectCollion r1w r1h (r1x, r1y) r2w r2h (r2x, r2y)
  | r1x < r2x + r2w &&
    r1x + r1w > r2x &&
    r1y < r2y + r2h &&
    r1h + r1y > r2y    = True
  | otherwise          = False


-- Out of Screen bounds checks if a bounding box shape is out of the screen bounds.
-- It returns a tuple that gives back (whether or not the shape is out of bounds, the possible return position)
-- The return position if you do not want the object to leave the space and need a place to put it back
outOfScreen :: (BoundingBox a, Moveable a) => a -> (Bool, Point)
outOfScreen object = outOfScreen' (getBox object) (position object)

outOfScreen' :: Box -> Point -> (Bool, Point)
outOfScreen' (CircleBox r) (x,y)
  | y > screenHeightHalf - r  = (True, (x, screenHeightHalf - r))
  | y < -screenHeightHalf + r = (True, (x, -screenHeightHalf + r))
  | x > screenWidthHalf - r   = (True, (screenWidthHalf - r, y))
  | x < -screenWidthHalf + r  = (True, (-screenWidthHalf + r, y))
  | otherwise                 = (False, (0, 0))
outOfScreen'  (RectangleBox w h) (x,y)
  | y > screenHeightHalf - halfH  = (True, (x, screenHeightHalf - halfH))
  | y < -screenHeightHalf + halfH = (True, (x, -screenHeightHalf + halfH))
  | x > screenWidthHalf - halfW   = (True, (screenWidthHalf - halfW, y))
  | x < -screenWidthHalf + halfW  = (True, (-screenWidthHalf + halfW, y))
  | otherwise                     = (False, (0, 0))
  where
    halfW = w / 2
    halfH = h / 2