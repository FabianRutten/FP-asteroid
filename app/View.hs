-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Space -> IO Picture
view = return . viewPure

viewPure :: Space -> Picture
viewPure s | paused s    == Paused   = viewPaused s
           | gameState s == GameOver = viewGameOver s
           | otherwise               = viewPlaying s

viewPaused :: Space -> Picture
viewPaused s = undefined

viewGameOver :: Space -> Picture
viewGameOver s = undefined

viewPlaying :: Space -> Picture
viewPlaying s = undefined

viewAll :: Space -> IO Picture
viewAll space = do
                player <- shipBMP
                return $ pictures [translate 200 200 player]

shipBMP :: IO Picture
shipBMP = loadBMP "ship.bmp"
