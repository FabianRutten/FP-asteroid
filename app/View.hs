{-# LANGUAGE InstanceSigs #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Model

-- load all bitmaps and call pure function
view :: Space -> IO Picture
view s = do
    backgroundBMP <- loadBMP "ship.bmp"
    playerBMP     <- loadBMP "ship.bmp"
    asteroidBMP   <- loadBMP "ship.bmp"
    saucerBMP     <- loadBMP "ship.bmp"
    bulletBMP     <- loadBMP "ship.bmp"
    return $ viewPure s [backgroundBMP, playerBMP, asteroidBMP, saucerBMP, bulletBMP]

-- choose correct view function based on the state of the game
viewPure :: Space -> [Picture] -> Picture
viewPure s | paused s    == Paused   = viewPaused s
           | gameState s == GameOver = viewGameOver s
           | otherwise               = viewPlaying s

viewPaused :: Space -> [Picture] -> Picture
viewPaused s bmps = pictures [renderSpace s bmps, color red (text "PAUSED")]

viewGameOver :: Space -> [Picture] -> Picture
viewGameOver s bmps = pictures [renderSpace s bmps, color red (text "GAME OVER")]

viewPlaying :: Space -> [Picture] -> Picture
viewPlaying = renderSpace

-- functions to alter a picture
translateToPosition :: Point -> Picture -> Picture
translateToPosition (x,y) = translate x y

rotateToOrientation :: Vector -> Picture -> Picture
rotateToOrientation v@(x,_) | x < 0 = Rotate $ - shift
                            | otherwise = Rotate shift
                    where
                        shift = degrees $ angleVV v (0,1)

scaleUniform :: Float -> Picture -> Picture
scaleUniform s = scale s s

-- Turn entire space into a picture by calling render on all relevant attributes with corresponding bitmaps
renderSpace :: Space -> [Picture] -> Picture
renderSpace s [backgroundBMP, playerBMP, asteroidBMP, saucerBMP, bulletBMP] 
    = pictures (background : playerPic : asteroidPics ++ saucerPics ++ bulletPics)
        where
            background   = backgroundBMP
            playerPic    = render    playerBMP   (player s)
            asteroidPics = mapRender asteroidBMP (asteroids s)
            saucerPics   = mapRender saucerBMP   (saucers s)
            bulletPics   = mapRender bulletBMP   (bullets s)
            mapRender bmp = map (render bmp)
renderSpace s _ = Blank -- go away non-exhaustive pattern match error

class Render a where
    render :: Picture -> a -> Picture

-- All instances to turn one of the attributes of the space into a picture
instance Render Player where
    render :: Picture -> Player -> Picture
    render bmp p = translateToPosition (position entity) $ rotateToOrientation (orientation p) $ scaleUniform (size entity) (pictures [bmp, color red $ text (show (position entity))])
        where
            entity = ship p

instance Render Asteroid where
    render :: Picture -> Asteroid -> Picture
    render bmp a = undefined

instance Render Saucer where
    render :: Picture -> Saucer -> Picture
    render bmp s = undefined

instance Render Bullet where
    render :: Picture -> Bullet -> Picture
    render bmp b =  translateToPosition (position entity) $ scaleUniform (size entity) (pictures [bmp, color red $ text (show (position entity))])
        where
            entity = projectile b