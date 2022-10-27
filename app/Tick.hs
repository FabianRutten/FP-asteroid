module Tick where

import Model
import Entity
import Data.Data (Data)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

screensize :: Float
screensize = 800
blackMargin :: Float
blackMargin = 100

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
                updateShipPosition = updateEntityPosition

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = updateEntityPosition

updateBullet :: Bullet -> Bullet
updateBullet = updateBulletPosition . updateBulletDistance
            where
                updateBulletPosition b = b {projectile = updateEntityPosition (projectile b)}
                updateBulletDistance b = b {distance = distance b + speed (projectile b)}

checkBulletsDistance :: [Bullet] -> [Bullet]
checkBulletsDistance = filter (\x -> distance x < (screensize/2))

updateSaucer :: Saucer -> Saucer
updateSaucer s = s

checkHits :: Space -> Space
checkHits s = s

checkPoint :: Point -> Point --screensize/2 + blackMargin (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint new@(x,y) | x > screen + blackMargin    = ((-screen)-blackMargin , y)
                     | y > screen + blackMargin    = ((-screen)-blackMargin , x)
                     | x < (-screen) - blackMargin = (screen+blackMargin    , y)
                     | y < (-screen) - blackMargin = (screen+blackMargin    , x)
                     | otherwise =  new
                where
                     screen = screensize/2

updateEntityPosition :: Entity -> Entity
updateEntityPosition a = a{ position = checkPoint (movePoint (position a) a)}
                where
                   movePoint p a = mulSV (speed a) (direction a) `addPoint` p