-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> Space -> IO Space
step secs space = undefined
  -- = -- We show a new random number
  --   do randomNumber <- randomIO
  --      let newNumber = abs randomNumber `mod` 10
  --      return $ GameState (ShowANumber newNumber) 0


-- | Handle user input
input :: Event -> Space -> IO Space
input e space = return (inputKey e space)

inputKey :: Event -> Space -> Space
inputKey = undefined
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same