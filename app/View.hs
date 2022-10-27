-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Distribution.Simple (UserHooks(postInst))
import Entity

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
viewAll s = return $ pictures [render $ player s]

class Render a where
    render :: a -> Picture

instance Render Player where
    render p = translateToPosition (position $ ship p) $ color white $ text $ show (position $ ship p) ++ show (speed $ ship p)--(text "A")

instance Render Asteroid where
    render a = undefined

instance Render Saucer where
    render s = undefined


translateToPosition :: Point -> Picture -> Picture
translateToPosition (x,y) = translate x y