-- | This module defines how the state changes
--   in response to time and user input
module Controller (step, input) where

import Model
import Tick ( updateTick )
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector ( magV, mulSV, normalizeV, rotateV )
import Animation ( Animation(running), activateAnimation )

-- | Handle one iteration of the game
step :: Float -> Space -> IO Space
step secs = return . stepPure

stepPure :: Space -> Space
stepPure space | static space = space                             -- do nothing if game is static
               | otherwise    = updateTick . alterPlayer $ space  -- first alter player based on movementkeys then update game

-- | Handle user input
input :: Event -> Space -> IO Space
-- impure save to file when player presses 's' after game is over
input (EventKey (Char 's') Down _ _) space@MkSpace{gameState = GameOver, saved = Unsaved} = saveScore space
-- handle all other input in pure function
input e space = return (inputKey e space)

saveScore :: Space -> IO Space
saveScore s = do
    appendFile "scores.txt" ("Your score was " ++ (show . score . player) s ++ "\n")
    return s {saved = Saved}

inputKey :: Event -> Space -> Space
inputKey (EventKey (Char key) Down _ _) s
    | key == 'p' = pause s                          -- pause/unpause when 'p' is pressed down
    | key == 'r' = restartGame s                    -- restart game
inputKey (EventKey (SpecialKey sk) state _ _) s
    | static s  = s                                 -- do nothing if game is static or if death animation is playing
    | otherwise = case sk of
        KeyLeft  -> setArrowkey 0 state s           -- update arrowkeysDown list if one of the relevant arrowkeys is pressed
        KeyUp    -> setArrowkey 1 state s
        KeyRight -> setArrowkey 2 state s
        KeySpace -> shootPlayer   state s           -- shoot on space
        _        -> s                               -- keep the same if no relevant special key is pressed
inputKey _ s = s                                    -- keep the same if no relevant key is pressed or no relevant event is called


-- change the bool in arrowkeysDown at a certain position based on the state of the key
setArrowkey :: Int -> KeyState -> Space -> Space
setArrowkey pos state s = s {arrowkeysDown = x ++ y : ys}
    where
        (x,_:ys) = splitAt pos (arrowkeysDown s)
        y = state == Down

-- game is static if it is paused or game is over, meaning space won't change anymore
static :: Space -> Bool
static s = paused s == Paused || gameState s == GameOver

-- flip paused state
pause :: Space -> Space
pause s@MkSpace{paused = Paused} = s {paused = Unpaused}
pause s                          = s {paused = Paused}

shootPlayer :: KeyState -> Space -> Space
shootPlayer Down s | (running . death . player) s = s
                   | otherwise                    = s {bullets = newBullet : bullets  s} -- only shoot when space is presed down and death animation is not playing
    where
        p = player s
        q = entityPlayer p
        startPoint = position q `addPoint` mulSV (size q) (direction q)
        newBullet = MkBullet newProjectile True 0
        newProjectile = MkEntity bulletSize startPoint (orientation p) bulletSpeed bulletRadius
shootPlayer _ s = s


fwdPlayer :: Float -> Player -> Player
fwdPlayer time p = p {entityPlayer = q {direction = normalizeV movementV, speed = magV movementV}, thrust = th}
    where
        q = entityPlayer p
        movementV = mulSV (speed q) (direction q) `addPoint` mulSV playerThrust (orientation p)
        th | running $ thrust p = thrust p
           | otherwise = activateAnimation time (thrust p)


rotatePlayer :: Float -> Player -> Player
rotatePlayer angle p = p {orientation = normalizeV $ rotateV angle (orientation p)}

restartGame :: Space -> Space
restartGame s = initialSpace $ randomSeed s

-- alter player attributes based on which arrowkey is pressed down according to arrowkeysDown
-- possible to hold multiple keys at the same time
-- called in step meaning it will keep updating the player if key is held down
alterPlayer :: Space -> Space
alterPlayer s | (running . death . player) s = s
              | otherwise = s {player = (newP fwd (fwdPlayer (time s)) . newP right (rotatePlayer (-playerRotateSpeed)) . newP left (rotatePlayer playerRotateSpeed)) p}
    where
        p = player s
        [left, fwd, right] = arrowkeysDown s
        newP b f = if b then f else id