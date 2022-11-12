module Saucer where

import Model
import Random
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector

pickNewDirection :: Saucer -> Space -> Vector
pickNewDirection s space | null as = chasePlayer s $ player space
                         | otherwise = evadeAsteroid s ca
    where
        as = asteroids space
        ca = closestAsteroid s as

evadeAsteroid :: Saucer -> Asteroid -> Vector
evadeAsteroid s a = undefined
    where
        es = entitySaucer s
        ea = entityAsteroid a  
        collisionDetected :: Saucer -> Asteroid
        collisionDetected = undefined
            where
                predictedAsteroidPostion :: Float -> Point
                predictedAsteroidPostion t | withinRange t = newAsteroidPosition t
                                           | otherwise      = predictedAsteroidPostion (t+1)
                    where
                        ap = position ea
                        spd = speed ea
                        dir = direction ea
                        sp = position es
                        newAsteroidPosition t = ap `addPoint` ((spd * t) `mulSV` dir)
                        distanceToNewAsteroid t = sp `distance2P` newAsteroidPosition t
                        withinRange :: Float -> Bool
                        withinRange t = abs ((distanceToNewAsteroid t / bulletSpeed) - t) < 1 --magic number



chasePlayer :: Saucer -> Player -> Vector
chasePlayer = undefined


saucerCalcBullet :: Player -> Saucer -> Bullet
saucerCalcBullet p s = MkBullet ( MkEntity bulletSize sp bulletVec bulletSpeed bulletRadius ) False 0
    where
        bulletVec = normalizeV $ predictedPlayerPosition 0 `minPoint` sp
        ep = entityPlayer p        
        sp = position $ entitySaucer s
        predictedPlayerPosition :: Float -> Point 
        predictedPlayerPosition t | withinRange t = newPlayerPosition t
                                  | otherwise     = predictedPlayerPosition (t+1)
            where                
                spd = speed ep
                dir = direction ep
                pp = position ep
                newPlayerPosition   t = pp `addPoint` ((spd * t )`mulSV` dir)
                distanceToNewPlayer t = sp `distance2P` newPlayerPosition t
                withinRange :: Float -> Bool
                withinRange t = abs ((distanceToNewPlayer t / bulletSpeed) - t) < 1 --magic number

closestAsteroid :: Saucer -> [Asteroid] -> Asteroid
closestAsteroid s as = ca
    where
        (ca, _) = smallest first rest
        (first:rest) = map (\x->  (x,distance2P (position $ entitySaucer s) (position $ entityAsteroid x))) as
        
        smallest :: (Asteroid, Float) -> [(Asteroid, Float)] -> (Asteroid, Float)
        smallest smoll [] = smoll 
        smallest smoll@(_,dist0) ((a1,dist1):xs) | dist0 <= dist1 = smallest smoll xs
                                                  | otherwise = smallest (a1,dist1) xs

