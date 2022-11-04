-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Tick

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Graphics.Gloss.Data.Vector

-- | Handle one iteration of the game
step :: Float -> Space -> IO Space
step secs space | static space = return space                               -- do nothing if game is static
                | otherwise    = return . updateTick . alterPlayer $ space  -- first alter player based on movementkeys then update game
--   do randomNumber <- randomIO
--      return $ GameState (ShowANumber (abs randomNumber `mod` 10)) 0


-- | Handle user input
input :: Event -> Space -> IO Space
input e space = return (inputKey e space)

inputKey :: Event -> Space -> Space
inputKey (EventKey key Down _ _) s 
    | key == Char 'p'          = pause s      -- pause/unpause when 'p' is pressed down
    | key == SpecialKey KeyEsc = escapeGame s -- escape game on esc
inputKey (EventKey (SpecialKey sk) state _ _) s
    | static s  = s                       -- do nothing if game is static
    | otherwise = case sk of
        KeyLeft  -> setArrowkey 0 state s -- update arrowkeysDown list if one of the relevant arrowkeys is pressed
        KeyUp    -> setArrowkey 1 state s
        KeyRight -> setArrowkey 2 state s
        KeySpace -> shootPlayer   state s -- shoot on space
        _        -> s                     -- keep the same if no relevant special key is pressed
inputKey _ s = s                          -- keep the same if no relevant key is pressed or no relevant event is called


-- change the bool in arrowkeysDown at a certain position based on the state of the key
setArrowkey :: Int -> KeyState -> Space -> Space
setArrowkey pos state s
  = let
        (x,_:ys) = splitAt pos (arrowkeysDown s)
        y = state == Down
    in
        s {arrowkeysDown = x ++ y : ys}

-- game is static if it is paused or game is over, meaning space won't change anymore
static :: Space -> Bool
static s = paused s == Paused || gameState s == GameOver

-- flip paused state
pause :: Space -> Space
pause s | paused s == Paused = s {paused = Unpaused}
        | otherwise          = s {paused = Paused}

shootPlayer :: KeyState -> Space -> Space
shootPlayer state s | state == Down = s {bullets = newBullet : bullets  s} -- only shoot when space is presed down
                    | otherwise     = s
      where
          p = player s
          q = entityPlayer p
          startPoint = position q `addPoint` mulSV (size q) (direction q)
          newBullet = MkBullet newProjectile True 0
          newProjectile = MkEntity 1 startPoint (orientation p) bulletSpeed bulletRadius


fwdPlayer :: Player -> Player
fwdPlayer p = p {entityPlayer = q {direction = normalizeV newMovementV, speed = magV newMovementV}}
            where
              q = entityPlayer p
              movementV = mulSV (speed q) (direction q)
              thrustV = mulSV playerThrust (orientation p)
              newMovementV = movementV `addPoint` thrustV


rotatePlayer :: Float -> Player -> Player
rotatePlayer angle p = p {orientation = normalizeV $ rotateV angle (orientation p)}

-- for now return to initalSpace, reset game
escapeGame :: Space -> Space
escapeGame s = initialSpace

-- alter player attributes based on which arrowkey is pressed down according to arrowkeysDown
-- possible to hold multiple keys at the same time
-- called in step meaning it will keep updating the player if key is held down
alterPlayer :: Space -> Space
alterPlayer s = let [left, fwd, right] = arrowkeysDown s
                    p = player s
                    playerLeft  | left      = rotatePlayer playerRotateSpeed p
                                | otherwise = p
                    playerRight | right     = rotatePlayer (-playerRotateSpeed) playerLeft
                                | otherwise = playerLeft
                    playerFwd   | fwd       = fwdPlayer playerRight
                                | otherwise = playerRight
                in s {player = playerFwd}