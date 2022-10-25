-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game lijst inlezen
step :: Float -> Space -> IO Space
step secs space | paused space = return space -- do nothing if paused
                | otherwise    = return space
  -- = -- We show a new random number
  --   do randomNumber <- randomIO
  --      let newNumber = abs randomNumber `mod` 10
  --      return $ GameState (ShowANumber newNumber) 0


-- | Handle user input lijst aanpassen
input :: Event -> Space -> IO Space
input e space = return (inputKey e space)

inputKey :: Event -> Space -> Space
inputKey (EventKey (Char 'p') _ _ _) s -- pause/unpause when 'p' is pressed
  = pause s
inputKey (EventKey (SpecialKey sk) _ _ _) s
  | paused s  = s                      -- no nothing if game is paused
  | otherwise = case sk of 
        KeySpace -> shoot s
        KeyUp    -> moveForward s
        KeyLeft  -> rotatePlayerLeft s
        KeyRight -> rotatePlayerRight s
        KeyEsc   -> escapeGame s
        _        -> s

inputKey _ s = s                      --keep the same if no relevant key is pressed or no relevant event
    --{ player = op sk (player space)}
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same

--pause the game
pause :: Space -> Space
pause s = s {paused = not $ paused s}

shoot :: Space -> Space
shoot s = undefined

moveForward :: Space -> Space
moveForward s = undefined

rotatePlayerLeft :: Space -> Space
rotatePlayerLeft s = undefined

rotatePlayerRight :: Space -> Space
rotatePlayerRight s = undefined

escapeGame :: Space -> Space
escapeGame s = undefined

updateTick :: Space -> Space
updateTick = undefined