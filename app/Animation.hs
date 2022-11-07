module Animation where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture


data Animation = MkAnimation { running :: Bool
                             , aframes :: [AFrame]} --only one animation active at a time

data AType = Death | Spawn | Thrust
    deriving (Eq, Show)

data AFrame = MkAFrame { picture :: Picture
                       , timing :: Float}   --timing < time space -> render Frame 

playerSpawnAnimation :: Animation
playerSpawnAnimation = MkAnimation False playerSpawnFrames

playerThrustAnimation :: Animation
playerThrustAnimation = playerSpawnAnimation

playerSpawnFrames :: [AFrame]
playerSpawnFrames = playerDeathFrames

playerDeathAnimation :: Animation
playerDeathAnimation = MkAnimation False playerDeathFrames

playerDeathFrames :: [AFrame]
playerDeathFrames = playerDeathFrames' [f0]
    where
        f (p,t) = MkAFrame (color white $ pictures p) t
        f0 = (pics, 0.2)
        pics = [l1,l2,l3]
        playerDeathFrames' :: [([Picture],Float)] -> [AFrame]
        playerDeathFrames' [] = []
        playerDeathFrames' (x:xs) | length xs == 4 = map f $ reverse (x:xs)
                            | otherwise = playerDeathFrames' (translateFrame x : (x:xs))
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

setAFramesTimes :: Float -> [AFrame] -> [AFrame]
setAFramesTimes f = map (\x-> x{timing = timing x + f})