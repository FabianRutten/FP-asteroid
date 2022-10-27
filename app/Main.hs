module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Asteroids" (screensize, screensize) (300, 100)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialSpace     -- Initial state
              viewAll          -- View function
              input            -- Event function
              step             -- Step function
    


