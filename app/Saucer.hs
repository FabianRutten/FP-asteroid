module Saucer where

import Model
import Random
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector


pickNewDirection :: Saucer -> Saucer
pickNewDirection = undefined

saucerCalcBullet :: Player -> Saucer -> Bullet
saucerCalcBullet p s = MkBullet ( MkEntity bulletSize sp bulletVec bulletSpeed bulletRadius ) False 0 
    where
        bulletVec = normalizeV $ recur 0 `minPoint` sp
        ep = entityPlayer p
        spd = speed ep
        dir = direction ep
        pp = position ep
        sp = position $ entitySaucer s
        recur :: Float -> Point 
        recur t | withinRange t = newPlayerPosition t
                | otherwise = recur (t+1)
            where
                newPlayerPosition t = pp `addPoint` ((spd * t )`mulSV` dir)
                distanceToNewPlayer t = sp `distance2P` newPlayerPosition t
                withinRange :: Float -> Bool
                withinRange t = abs ((distanceToNewPlayer t / bulletSpeed) - t) < 1

closestsFrontCollision :: Saucer -> [Asteroid] -> Point
closestsFrontCollision = undefined

