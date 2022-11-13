module Main where

import Controller ( input, step )
import Model ( frameRate, initialSpace, screensize )
import View ( view )

import Graphics.Gloss ( loadBMP )
import Graphics.Gloss.Interface.IO.Game ( black, Display(InWindow), playIO )
import System.Random ( getStdGen )

main :: IO ()
main = do
    backgroundBMP <- loadBMP "bitmaps/background.bmp"
    bulletBMP     <- loadBMP "bitmaps/bullet.bmp"
    asteroidBMP   <- loadBMP "bitmaps/asteroid.bmp"
    saucerBMP     <- loadBMP "bitmaps/saucer.bmp"
    playerBMP     <- loadBMP "bitmaps/player.bmp"
    gen           <- getStdGen
    let bmps = [backgroundBMP, bulletBMP, asteroidBMP, saucerBMP, playerBMP]
    playIO (InWindow "Asteroids" (screensize, screensize) (300, 100))
              black              -- Background color
              frameRate          -- Frames per second
              (initialSpace gen) -- Initial state
              (view bmps)        -- View function
              input              -- Event function
              step               -- Step function
    


