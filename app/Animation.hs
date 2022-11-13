module Animation where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Fixed (mod')

data Animation = MkAnimation { running   :: Bool
                             , frameFunc :: Float -> Picture -> Picture
                             , startTime :: Float
                             , duration  :: Float
                             }

--animations
playerSpawnAnimation :: Animation
playerSpawnAnimation = MkAnimation True playerSpawnFunc 0 2

playerThrustAnimation :: Animation
playerThrustAnimation = MkAnimation False playerThrustFunc 0 1

playerDeathAnimation :: Animation
playerDeathAnimation = MkAnimation False playerDeathFunc 0 1.5


playerSpawnFunc :: Float -> Picture -> Picture
playerSpawnFunc passed bmp | left >= 0.05 = Blank
                           | otherwise   = bmp
    where
        left = passed `mod'` 0.1

playerThrustFunc :: Float -> Picture -> Picture
playerThrustFunc passed bmp | left >= 0.1 = bmp 
                            | otherwise   = (color white . pictures) [bmp, thrustBMP]
    where
        left = passed `mod'` 0.2 
        thrustBMP = pictures [l1,l2]
            where
                l1 = line [(15,-35),(0,-55)]
                l2 = line [(-15,-35),(0,-55)]

playerDeathFunc :: Float -> Picture -> Picture
playerDeathFunc passed _ = (color white . pictures) $ translateFrames passed [l1,l2,l3]
    where
        --playerDeathFrames code
        l1 :: Picture
        l1 =  line [(-25,-25),(0,25)]
        l2 :: Picture
        l2 = line [(25,-25),(0,25)]
        l3 :: Picture
        l3 = line [(-15,-15),(15,-15)]
        translateFrames :: Float -> [Picture] -> [Picture]
        translateFrames t [g1,g2,g3] = [translate (-24*t) (24*t) g1
                                      , translate (24*t) (24*t) g2
                                      , translate 0 (-32*t)g3]
        translateFrames _ _ = [Blank] -- never called

activateAnimation :: Float -> Animation -> Animation
activateAnimation f a = a{running = True, startTime = f}
