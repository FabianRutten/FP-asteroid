{-# LANGUAGE InstanceSigs #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss hiding (animate)
import Graphics.Gloss.Data.Vector
import Model
import Animation
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Data.List (find, sort)
import Data.Maybe (isNothing, fromJust)
import System.Directory (doesFileExist)

view :: [Picture] -> Space -> IO Picture
view bmps s | gameState s == GameOver = viewGameOver s bmps
            | otherwise               = return $ viewPure s bmps

-- load and show highest scores
viewGameOver :: Space -> [Picture] -> IO Picture
viewGameOver s bmps = do
    highscore <- highscoreString
    let
        highscoreText = staticText (-290, 110)  0.4 aquamarine highscore
        gameOverText  = staticText (-395, -20)  1   red        "GAME OVER"
        restartText   = staticText (-250, -70)  0.3 magenta    "Press \"R\" to restart game"
        savedText     = staticText savedPoint   0.3 azure      savedString
        (savedPoint, savedString) | saved s == Unsaved = ((-240, -120), "Press \"S\" to save score")
                                  | otherwise          = ((-300, -120), "Succesfully saved score to file!")
    return $ pictures [renderSpace s bmps, gameOverText, restartText, savedText, highscoreText]

-- read all scores from file, get the highest and put it in a string, if there is no high score give empty string
highscoreString :: IO String
highscoreString = do
    file <- safeReadFile "highscores.txt"
    let scores = (reverse . sort . map (read . last . words)) (lines file)
    return $ highscoreString' scores
    where
        highscoreString' :: [Int] -> String
        highscoreString' []    = ""
        highscoreString' (x:_) = "Current Highscore: " ++ show x

        -- return content of file and if it doesn't exist return an empty string
        safeReadFile :: FilePath -> IO String
        safeReadFile fp = do
            exists <- doesFileExist fp
            let content | exists    = readFile fp
                        | otherwise = return ""
            content

-- choose correct view function based on the state of the game
viewPure :: Space -> [Picture] -> Picture
viewPure s | paused s    == Paused   = viewPaused s
           | otherwise               = viewPlaying s

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
            playerPic    = animate (time s) playerBMP   (player s)
            bulletPics   = render           bulletBMP   (bullets s)
            asteroidPics = render           asteroidBMP (asteroids s)
            saucerPics   = render           saucerBMP   (saucers s)
renderSpace s _ = Blank -- invalid lists render nothing

class Renderable a where
    render :: Picture -> a -> Picture

instance Renderable a => Renderable [a] where
    render :: Renderable a => Picture -> [a] -> Picture
    render bmp = pictures . map (render bmp)

instance Renderable Entity where
    render :: Picture -> Entity -> Picture
    render bmp e = pictures [transRotScale (position e) (direction e) (size e) bmp, debug]
        where
            debug = Blank --(transRotScale (position e) (direction e) 0.3 . color red . text . show . position) e


-- All instances to turn one of the attributes of the space into a picture
instance Renderable Player where
    render :: Picture -> Player -> Picture
    render bmp p = pictures [transRotScale (position e) (orientation p) (size e) bmp, showLives, showScore, debug]
        where
            e = entityPlayer p

            showScore = showLine (-390, 350) "Score:" (-270, 350)  score
            showLives = showLine (-390, 305) "Lives:" (-270, 305)  lives

            showLine p1 s p2 f = pictures [showText p1 s, showText p2 (show (f p))]
            showText p = staticText p 0.3 chartreuse
           
            debug = (transRotScale (position e) (orientation p) 0.3 . color red . text . show . running . death) p

instance Renderable Asteroid where
    render :: Picture -> Asteroid -> Picture
    render bmp a = render bmp (entityAsteroid a)

instance Renderable Saucer where
    render :: Picture -> Saucer -> Picture
    render bmp s = render bmp (entitySaucer s)

instance Renderable Bullet where
    render :: Picture -> Bullet -> Picture
    render bmp b =  render bmp (entityBullet b)

apicture :: Float -> [Animation] -> Picture
apicture f anims = pictures $ map (frame f) anims
          where
            frame :: Float -> Animation -> Picture
            frame f a | isNothing mFrame = Blank
                      | otherwise = picture $ fromJust mFrame
                        where
                            mFrame :: Maybe AFrame
                            mFrame = find (\x-> timing x > f) (aframes a)

class Renderable a => Animatable a where
    animate :: Float -> Picture -> a -> Picture

instance Animatable Player where
    animate :: Float -> Picture -> Player -> Picture
    animate secs bmp p | null animations = render bmp p
                       | otherwise = render (apicture secs animations) p
        where
            animations :: [Animation]
            animations = filter running [death p, spawn p, thrust p]