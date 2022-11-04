{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Tick where

import Model
import Collision
import Data.Data
import Data.Maybe
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import System.Random



updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

updateAsteroids :: Space -> Space
updateAsteroids s | null (asteroids s) = s {asteroids = spawnNew}
                  | otherwise = s {asteroids = map updateAsteroid $ asteroids s}
                where
                    spawnNew = spawnAsteroid --replicate numberInWave spawnAsteroid

spawnAsteroid :: [Asteroid]
spawnAsteroid = [MkAst $ MkEntity sizeBig    pickPoint pickDirectionB speedBig    $ asteroidRadius sizeBig,
                 MkAst $ MkEntity sizeMedium pickPoint pickDirectionM speedMedium $ asteroidRadius sizeMedium,
                 MkAst $ MkEntity sizeSmall  pickPoint pickDirectionS speedSmall  $ asteroidRadius sizeSmall]
           where
            xOry = True --for now, needs to be random
            pickPoint | xOry = (0,0)
                      | otherwise = (400,400)
            pickDirectionB = (1,4)
            pickDirectionM = (4,1)
            pickDirectionS = (2,0)

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ checkBulletsDistance $ bullets s}
                where
                    checkBulletsDistance :: [Bullet] -> [Bullet]
                    checkBulletsDistance = filter (\x -> distance x < halfscreen)
                    updateBullet :: Bullet -> Bullet
                    updateBullet = updateBulletPosition . updateBulletDistance
                           where
                        updateBulletPosition b = b {entityBullet = updateEntityPosition (entityBullet b)}
                        updateBulletDistance b = b {distance = distance b + speed (entityBullet b)}


updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {entityPlayer = (updateentityPlayerPosition . dragEntityPlayer) (entityPlayer p)}
                 in s {player = update $ player s}
            where
                updateentityPlayerPosition :: Entity -> Entity
                updateentityPlayerPosition = updateEntityPosition
                dragEntityPlayer :: Entity -> Entity
                dragEntityPlayer s = s {speed = max 0 (speed s - speed s * playerDrag / 100)}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a = a { entityAsteroid = updateEntityPosition $ entityAsteroid a }

updateSaucer :: Saucer -> Saucer
updateSaucer s = s


checkPoint :: Point -> Point --screensize/2 + floatBlackMargin (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint old@(x,y) | x >  (halfscreen + floatBlackMargin)    = ((-halfscreen) -floatBlackMargin, y)
                     | y >  (halfscreen + floatBlackMargin)    = (x, (-halfscreen)-floatBlackMargin)
                     | x <  ((-halfscreen) - floatBlackMargin) = (halfscreen+floatBlackMargin, y)
                     | y <  ((-halfscreen) - floatBlackMargin) = (x, halfscreen+floatBlackMargin)
                     | otherwise = old

--first update position, simply with direction and speed. 
--Then check if new position is outside of set bounderies of the play space and 
updateEntityPosition :: Entity -> Entity
updateEntityPosition a = a{ position = checkPoint (movePoint a)}
                where
                    movePoint :: Entity -> Point
                    movePoint a = mulSV (speed a) (direction a) `addPoint` position a






