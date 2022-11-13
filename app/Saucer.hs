module Saucer (saucerAi, secondsBetweenMan, secondsBetweenShot) where

import Model
import Random
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector
import System.Random (StdGen)
import Graphics.Gloss.Geometry.Angle (degToRad)

--constants in algos
shootingSkill :: Float --only when moving
shootingSkill = 1--the lower, the better. Best = 1

evasionSkillAngle :: Float
evasionSkillAngle = 50

evasionSkill :: Float
evasionSkill = 1

aimNerf :: Float
aimNerf = 5

secondsBetweenMan :: Float
secondsBetweenMan = 0.1

secondsBetweenShot :: StdGen -> (Float,StdGen)
secondsBetweenShot gen = randomInRange gen (2, 5)

saucerAi :: Space -> Space
saucerAi s = updateSaucers s{saucers = map newSaucer srcs, bullets = newBullets, randomSeed = newestSeed }
    where
        srcs = saucers s
        newSaucer :: Saucer -> Saucer
        newSaucer src | lastManeuver src > secondsBetweenMan = pickNewDirection src s
                      | otherwise = src
        (newBullets,newestSeed) = foldr f (bullets s, randomSeed s) srcs
            where
                f sauce (blts, gen) | lastShot sauce > dur = (newB : blts, gen2)
                                    | otherwise = (blts,gen1)
                    where
                        (newB, gen2) = saucerCalcBullet gen (player s) sauce
                        (dur, gen1) = secondsBetweenShot gen

updateSaucers :: Space -> Space
updateSaucers s = s{randomSeed = newSeed,saucers = map (\x-> x{lastManeuver = addSecs secondsBetweenMan (lastManeuver x) , lastShot = addSecs dur (lastShot x)}) (saucers s)}
    where
        (dur,newSeed) = secondsBetweenShot (randomSeed s)
        addSecs :: Float -> Float -> Float
        addSecs max a | a < max = a + (1 / fromIntegral frameRate )
                      | otherwise = 0

pickNewDirection :: Saucer -> Space -> Saucer
pickNewDirection s space = evadeAsteroid (player space) s ca
    where
        as = asteroids space
        ca = closestAsteroid s as

evadeAsteroid :: Player -> Saucer -> Asteroid -> Saucer
evadeAsteroid p s a = s{entitySaucer = es{direction = newCourse}}
    where
        es = entitySaucer s
        ea = entityAsteroid a
        dir = direction ea
        newCourse :: Vector
        newCourse | onCollision && goLeft = normalizeV $ rotateV (degToRad 45) dir
                  | onCollision && not goLeft = normalizeV $ rotateV (degToRad (-45)) dir
                  | otherwise = chasePlayer s p 
            where
                goLeft = angleVV dir lVec <= angleVV dir rVec
                onCollision = angleVV dir collisionCourse < maxAngle
                maxAngle = angleVV dir lVec
                lBoundery = angleVV dir
                lVec = ap `addPoint` pVec
                rVec = ap `addPoint` rotateV pi pVec
                pVec = normalizeV $ rotateV (pi/2) collisionCourse
                sp = position es
                ap = position ea
                collisionCourse :: Vector
                collisionCourse = predictedAsteroidPostion 0 `minPoint` sp
                predictedAsteroidPostion :: Float -> Point
                predictedAsteroidPostion t | withinRange t = newAsteroidPosition t
                                           | otherwise      = predictedAsteroidPostion (t+1)
                    where              
                            newAsteroidPosition t = ap `addPoint` ((spd * t) `mulSV` dir)
                            distanceToNewAsteroid t = sp `distance2P` newAsteroidPosition t
                            withinRange :: Float -> Bool
                            withinRange t = abs ((distanceToNewAsteroid t / bulletSpeed) - t) < evasionSkill                
                            spd = speed ea



chasePlayer :: Saucer -> Player -> Vector
chasePlayer s p = newDir
    where
        es = entitySaucer s
        newDir = normalizeV $ rotateV (degToRad 20) $ position (entityPlayer p) `minPoint` position es


saucerCalcBullet :: StdGen -> Player -> Saucer -> (Bullet,StdGen)
saucerCalcBullet gen p s = (MkBullet ( MkEntity bulletSize (sp `addPoint` (saucerRadius `mulSV` bulletVec) )bulletVec bulletSpeed bulletRadius ) False 0, newSeed)
    where
        (randomAngle,newSeed) = randomInRange gen (-aimNerf,aimNerf)
        bulletVec = rotateV (degToRad randomAngle) $ normalizeV $ predictedPlayerPosition 0 `minPoint` sp
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
                withinRange t = abs ((distanceToNewPlayer t / bulletSpeed) - t) < shootingSkill

closestAsteroid :: Saucer -> [Asteroid] -> Asteroid
closestAsteroid s as = ca
    where
        (ca, _) = smallest first rest
        (first:rest) = map (\x->  (x,distance2P (position $ entitySaucer s) (position $ entityAsteroid x))) as

        smallest :: (Asteroid, Float) -> [(Asteroid, Float)] -> (Asteroid, Float)
        smallest smoll [] = smoll
        smallest smoll@(_,dist0) ((a1,dist1):xs) | dist0 <= dist1 = smallest smoll xs
                                                  | otherwise = smallest (a1,dist1) xs

