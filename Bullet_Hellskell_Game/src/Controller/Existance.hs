-- Imeplents a function for everything that exists.
-- When object that exist are in a list, the function will
-- remove any which are dead from that list.
module Controller.Existance where

import Model.Existance

removeEntities :: (Existance a) => [a] -> [a]
removeEntities = filter isAlive