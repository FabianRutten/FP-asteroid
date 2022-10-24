-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Space -> IO Picture
view = return . viewPure

viewPure :: Space -> Picture
viewPure gstate = undefined