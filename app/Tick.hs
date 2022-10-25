module Tick where

import Model



updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkHits

updateAsteroids :: Space -> Space
updateAsteroids s = s {asteroids = map updateAsteroid $ asteroids s}

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ bullets s}

updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = s {player = update $ player s}
        where
            update p = p

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = undefined

updateBullet :: Bullet -> Bullet
updateBullet = undefined

updateSaucer :: Saucer -> Saucer
updateSaucer = undefined

checkHits :: Space -> Space
checkHits = undefined
