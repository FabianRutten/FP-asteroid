module Tick where

import Model
import Data.Data (Data)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import System.Random



updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

updateAsteroids :: Space -> Space
updateAsteroids s | score (player s) == 0 && null (asteroids s) = s {asteroids = spawnNew}
                  | score (player s) `elem` nextWaveScores = s {asteroids = spawnNew}
                  | otherwise = s {asteroids = map updateAsteroid $ asteroids s}
                where
                    spawnNew = replicate numberInWave spawnAsteroid

spawnAsteroid :: Asteroid
spawnAsteroid = MkAst $ MkEntity sizeBig pickPoint pickDirection speedBig
           where
            xOry = True --for now, needs to be random
            pickPoint | xOry = (0,0)
                      | otherwise = (400,400)
            pickDirection = (1,4)

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ checkBulletsDistance $ bullets s}
                where
                    checkBulletsDistance :: [Bullet] -> [Bullet]
                    checkBulletsDistance = filter (\x -> distance x < halfscreen)


updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {ship = (updateShipPosition . dragShip) (ship p)}
                 in s {player = update $ player s}
            where
                updateShipPosition :: Entity -> Entity
                updateShipPosition = updateEntityPosition
                dragShip :: Entity -> Entity
                dragShip s = s {speed = max 0 (speed s - speed s * playerDrag / 100)}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a = MkAst $ updateEntityPosition $ entityAsteroid a

updateBullet :: Bullet -> Bullet
updateBullet = updateBulletPosition . updateBulletDistance
            where
                updateBulletPosition b = b {projectile = updateEntityPosition (projectile b)}
                updateBulletDistance b = b {distance = distance b + speed (projectile b)}



updateSaucer :: Saucer -> Saucer
updateSaucer s = s

checkCollisions :: Space -> Space
checkCollisions s = s

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