module Animation where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture


data Animation = MkAnimation { running :: Bool
                             , frameFunc :: Float -> Picture -> Picture
                             , startTime :: Float} --only one animation active at a time

playerSpawnAnimation :: Animation
playerSpawnAnimation = MkAnimation True playerSpawnFunc 0

playerSpawnFunc :: Float -> Picture -> Picture
playerSpawnFunc = undefined

playerThrustAnimation :: Animation
playerThrustAnimation = MkAnimation False playerThrustFunc 0

playerThrustFunc :: Float -> Picture -> Picture
playerThrustFunc = undefined

playerDeathAnimation :: Animation
playerDeathAnimation = MkAnimation False playerDeathFunc 0

playerDeathFunc :: Float -> Picture -> Picture
playerDeathFunc f p = 
        --playerDeathFrames code
        l1 :: Picture
        l1 = line [(-25,-25),(0,25)]
        l2 :: Picture
        l2 = line [(25,-25),(0,25)]
        l3 :: Picture
        l3 = line [(-15,-15),(15,-15)]
        translateFrame :: ([Picture],Float) -> ([Picture],Float)
        translateFrame ([g1,g2,g3],x) = ([translate (-3) 3 g1
                                        , translate 3 3 g2
                                        , translate 0 (-4) g3]
                                       , x + 0.2)
        translateFrame _ = ([], 0)

activateAnimation :: Float -> Animation -> Animation
activateAnimation f a = a{running = True, startTime = f}
