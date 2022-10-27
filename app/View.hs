-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Distribution.Simple (UserHooks(postInst))
import Entity (Entity(position))

view :: Space -> IO Picture
--view = return . viewPure
view _ = undefined

viewPure :: Space -> Picture
viewPure gstate = undefined

viewAll :: Space -> IO Picture
viewAll space = do
                player <- playerPicture (player space)
                return $ pictures [player]


playerPicture :: Player -> IO Picture
playerPicture p = return $ translateToPosition (position $ ship p) $ color white (text "A")





translateToPosition :: Point -> Picture -> Picture
translateToPosition (x,y) = translate x y