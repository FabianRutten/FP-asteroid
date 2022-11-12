module Animation where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Fixed (mod')


data Animation = MkAnimation { running :: Bool
                             , frameFunc :: Float -> Float -> Picture -> Picture
                             , startTime :: Float
                             , duration :: Float}

playerSpawnAnimation :: Animation
playerSpawnAnimation = MkAnimation True playerSpawnFunc 0 2

playerSpawnFunc :: Float -> Float -> Picture -> Picture
playerSpawnFunc st secs bmp | left >= 0.1 = Blank
                            | otherwise = bmp
    where
        left = secs - st `mod'` 0.2


playerThrustAnimation :: Animation
playerThrustAnimation = MkAnimation False playerThrustFunc 0 0.1

playerThrustFunc :: Float -> Float -> Picture -> Picture
playerThrustFunc = undefined

playerDeathAnimation :: Animation
playerDeathAnimation = MkAnimation False playerDeathFunc 0 1.5

playerDeathFunc :: Float -> Float -> Picture -> Picture
playerDeathFunc st secs _ = pictures $ translateFrames passedTime [l1,l2,l3]
    where
        --playerDeathFrames code
        passedTime = secs - st
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
