module Tick where

import Model
import Entity
import Data.Data (Data)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector



updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkHits

updateAsteroids :: Space -> Space
updateAsteroids s = s {asteroids = map updateAsteroid $ asteroids s}

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ checkBulletsDistance $ bullets s}
                where 
                    checkBulletsDistance :: [Bullet] -> [Bullet]
                    checkBulletsDistance = filter (\x -> distance x < halfscreen)

                    
updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {ship = updateShipPosition (ship p)}
                 in s {player = update $ player s}
            where
                updateShipPosition = updateEntityPosition

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = updateEntityPosition

updateBullet :: Bullet -> Bullet
updateBullet = updateBulletPosition . updateBulletDistance
            where
                updateBulletPosition b = b {projectile = updateEntityPosition (projectile b)}
                updateBulletDistance b = b {distance = distance b + speed (projectile b)}



updateSaucer :: Saucer -> Saucer
updateSaucer s = s

checkHits :: Space -> Space
checkHits s = s

checkPoint :: Point -> Point --screensize/2 + floatBlackMargin (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint new@(x,y) | x >  (halfscreen + floatBlackMargin)    = ((-halfscreen) -floatBlackMargin , y)
                     | y >  (halfscreen + floatBlackMargin)    =  ((-halfscreen)-floatBlackMargin , x)
                     | x <  ((-halfscreen) - floatBlackMargin) = (halfscreen+floatBlackMargin     , y)
                     | y <  ((-halfscreen) - floatBlackMargin) = (halfscreen+floatBlackMargin     , x)
                     | otherwise =  new

--first update position, simply with direction and speed. 
--Then check if new position is outside of set bounderies of the play space and 
updateEntityPosition :: Entity -> Entity
updateEntityPosition a = a{ position = checkPoint (movePoint a)}
                where
                    movePoint :: Entity -> Point
                    movePoint a = mulSV (speed a) (direction a) `addPoint` position a