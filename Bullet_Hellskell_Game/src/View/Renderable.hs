-- Module for things thay can be rendered
module View.Renderable where

import Model.Moveable

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- Type class for things in the game that can be rendered
-- Things that can be rendered also have a shape. Since we only 
-- use geometric shapes.
class (Moveable a) => Renderable a where
    render      :: a -> Picture
    shape       :: a -> Picture
