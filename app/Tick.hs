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

updateShipPosition :: Entity -> Entity
updateShipPosition ship = let newPoint = newShipPoint (position ship) ship
                          in ship {position = updatePoint newPoint}
                          where
                              newShipPoint p ship = mulSV (speed ship) (direction ship) `addPoint` p


updatePoint :: Point -> Point --screensize/2 + 100 (dark place) as maximum point values. with negatives as well -> swap sides
updatePoint new@(x,y) | x > screen + 100    = ((-screen)-100 , y)
                      | y > screen + 100    = ((-screen)-100 , x)
                      | x < (-screen) - 100 = (screen+100 , y)
                      | y < (-screen) - 100 = (screen+100, x)
                      | otherwise =  new
                where
                    screen = screensize/2

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = undefined

updateBullet :: Bullet -> Bullet
updateBullet = undefined

updateSaucer :: Saucer -> Saucer
updateSaucer = undefined

checkHits :: Space -> Space
checkHits = undefined
