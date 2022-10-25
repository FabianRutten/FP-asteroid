-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Bitmap

view :: Space -> IO Picture
--view = return . viewPure
view _ = shipBMP

viewPure :: Space -> Picture
viewPure gstate = undefined

viewAll :: Space -> IO Picture
viewAll space = do
                player <- shipBMP
                return $ pictures [translate 200 200 player]