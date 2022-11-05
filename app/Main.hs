module Main where

import Controller
import Model
import View

import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Game
import System.Random (getStdGen)

main :: IO ()
main = do
    backgroundBMP <- loadBMP "bitmaps/background.bmp"
    bulletBMP     <- loadBMP "bitmaps/bullet.bmp"
    asteroidBMP   <- loadBMP "bitmaps/asteroid.bmp"
    saucerBMP     <- loadBMP "bitmaps/saucer.bmp"
    playerBMP     <- loadBMP "bitmaps/player.bmp"
    gen           <- getStdGen
    playIO (InWindow "Asteroids" (screensize, screensize) (300, 100))
              black                                                                 -- Background color
              30                                                                    -- Frames per second
              (initialSpace gen)                                                    -- Initial state
              (view [backgroundBMP, bulletBMP, asteroidBMP, saucerBMP, playerBMP])  -- View function
              input                                                                 -- Event function
              step                                                                  -- Step function
    


