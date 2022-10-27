module Tick where

import Model
import Entity
import Data.Data (Data)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

screensize :: Float
screensize = 800


updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkHits

updateAsteroids :: Space -> Space
updateAsteroids s = s {asteroids = map updateAsteroid $ asteroids s}

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ bullets s}

updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {ship = updateShipPosition (ship p)}
                 in s {player = update $ player s}
            where
                updateShipPosition = updatePointEntity

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = updatePointEntity

updateBullet :: Bullet -> Bullet
updateBullet = updateBulletPosition . updateBulletDistance
            where 
                updateBulletPosition b = b {projectile = updatePointEntity (projectile b)}
                updateBulletDistance = undefined

updateSaucer :: Saucer -> Saucer
updateSaucer s = s

checkHits :: Space -> Space
checkHits s = s

checkPoint :: Point -> Point --screensize/2 + 100 (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint new@(x,y) | x > screen + 100    = ((-screen)-100 , y)
                     | y > screen + 100    = ((-screen)-100 , x)
                     | x < (-screen) - 100 = (screen+100    , y)
                     | y < (-screen) - 100 = (screen+100    , x)
                     | otherwise =  new
                where
                     screen = screensize/2

updatePointEntity :: Entity -> Entity
updatePointEntity a = a{ position = checkPoint (movePoint (position a) a)}
                where
                   movePoint p a = mulSV (speed a) (direction a) `addPoint` p