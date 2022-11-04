module Main where

import Controller
import Model
import View

import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    backgroundBMP <- loadBMP "bitmaps/background.bmp"
    bulletBMP     <- loadBMP "bitmaps/bullet.bmp"
    asteroidBMP   <- loadBMP "bitmaps/asteroid.bmp"
    saucerBMP     <- loadBMP "bitmaps/ship.bmp"
    playerBMP     <- loadBMP "bitmaps/ship.bmp"
    playIO (InWindow "Asteroids" (screensize, screensize) (300, 100)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialSpace     -- Initial state
              (view [backgroundBMP, bulletBMP, asteroidBMP, saucerBMP, playerBMP])      -- View function
              input            -- Event function
              step             -- Step function
    


