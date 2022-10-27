-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Space -> IO Picture
--view = return . viewPure
view _ = undefined

viewPure :: Space -> Picture
viewPure gstate = undefined

viewAll :: Space -> IO Picture
viewAll space = do
                player <- playerBMP (player space)
                return $ pictures [player]


playerBMP :: Player -> IO Picture
playerBMP = undefined 