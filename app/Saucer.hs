module Saucer (saucerAi, secondsBetweenMan, secondsBetweenShot) where 

import Model
import Random
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector

--constants in algos
evasionSkill :: Float
evasionSkill = 1

secondsBetweenMan :: Float
secondsBetweenMan = 0.1

secondsBetweenShot :: Float
secondsBetweenShot = 2

saucerAi :: Space -> Space
saucerAi s = s{saucers = map newSaucer srcs, bullets = newBullets }
    where
        srcs = saucers s
        newSaucer :: Saucer -> Saucer
        newSaucer src | lastManeuver src > secondsBetweenMan = pickNewDirection src s
                      | otherwise = src
        newBullets :: [Bullet]
        newBullets = foldr f (bullets s) srcs
            where
                f sauce blts | lastShot sauce > secondsBetweenShot = saucerCalcBullet (player s) sauce : blts
                             | otherwise = blts

pickNewDirection :: Saucer -> Space -> Saucer
pickNewDirection s space | null as = chasePlayer s $ player space
                         | otherwise = evadeAsteroid s ca
    where
        as = asteroids space
        ca = closestAsteroid s as

evadeAsteroid :: Saucer -> Asteroid -> Saucer
evadeAsteroid s a | collisionDetected = s{entitySaucer = es{direction = newDirection}}
                  | otherwise = s
    where
        newDirection = rotateV 45 dir
        es = entitySaucer s
        ea = entityAsteroid a       
        dir = direction ea
        collisionDetected :: Bool
        collisionDetected = angleVV (direction es) collisionCourse < evasionSkill
            where                
                sp = position es
                collisionCourse :: Vector
                collisionCourse = predictedAsteroidPostion 0 `minPoint` sp
                predictedAsteroidPostion :: Float -> Point
                predictedAsteroidPostion t | withinRange t = newAsteroidPosition t
                                           | otherwise      = predictedAsteroidPostion (t+1)
                    where
                        ap = position ea
                        spd = speed ea
                        newAsteroidPosition t = ap `addPoint` ((spd * t) `mulSV` dir)
                        distanceToNewAsteroid t = sp `distance2P` newAsteroidPosition t
                        withinRange :: Float -> Bool
                        withinRange t = abs ((distanceToNewAsteroid t / bulletSpeed) - t) < 1 --magic number



chasePlayer :: Saucer -> Player -> Saucer
chasePlayer s _ = s


saucerCalcBullet :: Player -> Saucer -> Bullet
saucerCalcBullet p s = MkBullet ( MkEntity bulletSize (sp `addPoint` (saucerRadius `mulSV` bulletVec) )bulletVec bulletSpeed bulletRadius ) False 0
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

