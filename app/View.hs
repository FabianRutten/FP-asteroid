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
viewPaused s = pictures [color white (text "PAUSED"), render s]

viewGameOver :: Space -> Picture
viewGameOver s = pictures [color white (text "GAME OVER"), render s]

viewPlaying :: Space -> Picture
viewPlaying = render


translateToPosition :: Point -> Picture -> Picture
translateToPosition (x,y) = translate x y



class Render a where
    render :: a -> Picture

-- Turn entire space into a picture by calling render on all relevant attributes
instance Render Space where
    render s = pictures (render (player s) : map render (asteroids s) ++ map render (saucers s) ++ map render (bullets s))

-- All instances to turn one of the attributes of the space into a picture
instance Render Player where
    render p = translateToPosition (position $ ship p) (color white $ text $ show (position $ ship p) ++ show (speed $ ship p))--(text "A")

instance Render Asteroid where
    render a = undefined

instance Render Saucer where
    render s = undefined

instance Render Bullet where
    render b = undefined