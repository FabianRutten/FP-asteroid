module Tick where

import Model
import Entity (Entity)
import Data.Data (Data)


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
                     go = undefined
                in s {player = update $ player s}
                
updateShipPosition :: Entity -> Entity
updateShipPosition = undefined

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = undefined

updateBullet :: Bullet -> Bullet
updateBullet = undefined

updateSaucer :: Saucer -> Saucer
updateSaucer = undefined

checkHits :: Space -> Space
checkHits = undefined
