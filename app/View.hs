{-# LANGUAGE InstanceSigs #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Model
import Graphics.Gloss.Geometry.Angle (radToDeg)

-- load all bitmaps and call pure function
view :: [Picture] -> Space -> IO Picture
view bmps s = return $ viewPure s bmps

-- choose correct view function based on the state of the game
viewPure :: Space -> [Picture] -> Picture
viewPure s | gameState s == GameOver = viewGameOver s
           | paused s    == Paused   = viewPaused s
           | otherwise               = viewPlaying s

viewGameOver :: Space -> [Picture] -> Picture
viewGameOver s bmps = pictures [head bmps, render (last bmps) (player s), gameOverText, restartText, savedText]
    where
        gameOverText = staticText (-395, -20) 1   red     "GAME OVER"
        restartText  = staticText (-250, -70) 0.3 magenta "Press \"R\" to restart game"
        savedText    = staticText savedPoint  0.3 azure   savedString
        (savedPoint, savedString) | saved s == Unsaved = ((-240, -120), "Press \"S\" to save score")
                                  | otherwise          = ((-300, -120), "Succesfully saved score to file!")

viewPaused :: Space -> [Picture] -> Picture
viewPaused s bmps = pictures [renderSpace s bmps, staticText (-240, -20) 1 red "PAUSED"]

viewPlaying :: Space -> [Picture] -> Picture
viewPlaying = renderSpace

-- functions to alter a picture
transRotScale :: Point -> Vector -> Float -> Picture -> Picture
transRotScale p v f = translateToPosition p  . rotateToOrientation v  . scaleUniform f

translateToPosition :: Point -> Picture -> Picture
translateToPosition (x,y) = translate x y

rotateToOrientation :: Vector -> Picture -> Picture
rotateToOrientation v@(x,_) | x < 0     = rotate (-shift)
                            | otherwise = rotate shift
                    where
                        shift = radToDeg $ angleVV v (0,1)

scaleUniform :: Float -> Picture -> Picture
scaleUniform s = scale s s

-- make picture from string at given point, scale and color
staticText :: Point -> Float -> Color -> String -> Picture
staticText p s c = translateToPosition p . scaleUniform s . color c . text

-- Turn entire space into a picture by calling render on all relevant attributes with corresponding bitmaps
renderSpace :: Space -> [Picture] -> Picture
renderSpace s [backgroundBMP, bulletBMP, asteroidBMP, saucerBMP, playerBMP]
    = pictures [background, bulletPics, asteroidPics, saucerPics, playerPic]
        where
            background   = backgroundBMP
            playerPic    = render playerBMP   (player s)
            bulletPics   = render bulletBMP   (bullets s)
            asteroidPics = render asteroidBMP (asteroids s)
            saucerPics   = render saucerBMP   (saucers s)
renderSpace s _ = Blank -- invalid lists render nothing

class Render a where
    render :: Picture -> a -> Picture

instance Render a => Render [a] where
    render :: Render a => Picture -> [a] -> Picture
    render bmp = pictures . map (render bmp)

instance Render Entity where
    render :: Picture -> Entity -> Picture
    render bmp e = pictures [transRotScale (position e) (direction e) (size e) bmp, debug]
        where
            debug = Blank --(transRotScale (position e) (direction e) 0.3 . color red . text . show . position) e
            

-- All instances to turn one of the attributes of the space into a picture
instance Render Player where
    render :: Picture -> Player -> Picture
    render bmp p = pictures [transRotScale (position e) (orientation p) (size e) bmp, showLives, showScore, debug]
        where
            e = entityPlayer p
           
            showScore = showLine (-390, 350) "Score:" (-270, 350)  score
            showLives = showLine (-390, 305) "Lives:" (-270, 305)  lives

            showLine p1 s p2 f = pictures [showText p1 s, showText p2 (show (f p))]
            showText p = staticText p 0.3 chartreuse
           
            debug = Blank --(transRotScale (position e) (orientation p) 0.3 . color red . text . show . orientation) p

instance Render Asteroid where
    render :: Picture -> Asteroid -> Picture
    render bmp a = render bmp (entityAsteroid a)

instance Render Saucer where
    render :: Picture -> Saucer -> Picture
    render bmp s = render bmp (entitySaucer s)

instance Render Bullet where
    render :: Picture -> Bullet -> Picture
    render bmp b =  render bmp (entityBullet b)