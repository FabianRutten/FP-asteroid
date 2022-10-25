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
                | otherwise    = return . updateTick . alterPlayer $ space -- first alter player based on movementkeys then update game
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
inputKey (EventKey (SpecialKey sk) state _ _) s
  | paused s  = s                      -- do nothing if game is paused
  | otherwise = case sk of 
        KeyLeft  -> setArrowkey 0 state s
        KeyUp    -> setArrowkey 1 state s
        KeyRight -> setArrowkey 2 state s
        KeySpace -> shoot s
        KeyEsc   -> escapeGame s
        _        -> s                  -- keep the same if no relevant special key is pressed
inputKey _ s = s                       -- keep the same if no relevant key is pressed or no relevant event is called


-- change the bool in arrowkeysDown at a certain position based on the state of the key
setArrowkey :: Int -> KeyState -> Space -> Space
setArrowkey pos state s 
  = let 
      (x,_:ys) = splitAt pos (arrowkeysDown s) 
      y = state == Down
    in 
      s {arrowkeysDown = x ++ y : ys}

-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same

--pause the game
pause :: Space -> Space
pause s = s {paused = not $ paused s}

shoot :: Space -> Space
shoot s = undefined

movePlayerForward :: Player -> Player
movePlayerForward p = undefined

rotatePlayerLeft :: Player -> Player
rotatePlayerLeft p = undefined

rotatePlayerRight ::Player -> Player
rotatePlayerRight p = undefined

escapeGame :: Space -> Space
escapeGame s = undefined

updateTick :: Space -> Space
updateTick = undefined

alterPlayer :: Space -> Space
alterPlayer s = let [left, fwd, right] = arrowkeysDown s
                    p = player s
                    playerLeft  | left      = rotatePlayerLeft p 
                                | otherwise = p
                    playerRight | right     = rotatePlayerRight playerLeft
                                | otherwise = playerLeft
                    playerFwd   | fwd       = movePlayerForward playerRight
                                | otherwise = playerRight
                in s {player = playerFwd}