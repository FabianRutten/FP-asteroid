{-# LANGUAGE InstanceSigs #-}
module Tick where

import Model
import Collision ( checkCollisions )
import Graphics.Gloss.Data.Point ( Point )
import Graphics.Gloss.Data.Vector ( mulSV )

updateTick :: Space -> Space
updateTick =  update . spawnAsteroids . checkCollisions --updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

spawnAsteroids :: Space -> Space
spawnAsteroids s | null (asteroids s) = s { asteroids = spawnAsteroid }--replicate numberInWave spawnAsteroid
                 | otherwise = s
    where
        spawnAsteroid :: [Asteroid]
        spawnAsteroid = [MkAst $ MkEntity sizeBig    pickPoint pickDirectionB speedBig    $ asteroidRadius sizeBig,
                         MkAst $ MkEntity sizeMedium pickPoint pickDirectionM speedMedium $ asteroidRadius sizeMedium,
                         MkAst $ MkEntity sizeSmall  pickPoint pickDirectionS speedSmall  $ asteroidRadius sizeSmall]
            where
                pickPoint      = (400,400)
                pickDirectionB = (1,4)
                pickDirectionM = (4,1)
                pickDirectionS = (2,0)

class Update a where
    update :: a -> a

instance Update a => Update [a] where 
    update :: Update a => [a] -> [a]
    update = map update

instance Update Space where 
    update :: Space -> Space
    update s = s { player    = update (player s), 
                   asteroids = update (asteroids s), 
                   saucers   = update (saucers s),
                   bullets   = update (filter (\x -> distance x < halfscreen) (bullets s))
                 }

instance Update Entity where
    update :: Entity -> Entity
    update e = e { position = checkPoint $ mulSV (speed e) (direction e) `addPoint` position e}
        where
            -- check if a given point is outside the window, and make it swap sides if it is
            checkPoint :: Point -> Point --screensize/2 + blackMargin (dark place) as maximum point values. with negatives as well -> swap sides
            checkPoint p@(x,y) | x >  (halfscreen + blackMargin)    = ((-halfscreen) -blackMargin, y)
                               | y >  (halfscreen + blackMargin)    = (x, (-halfscreen)-blackMargin)
                               | x <  ((-halfscreen) - blackMargin) = (halfscreen+blackMargin, y)
                               | y <  ((-halfscreen) - blackMargin) = (x, halfscreen+blackMargin)
                               | otherwise = p

instance Update Player where
    update :: Player -> Player
    update p = p {entityPlayer = (update . drag) (entityPlayer p)}
        where
            drag e = e {speed = max 0 (speed e - speed e * playerDrag)}

instance Update Asteroid where
    update :: Asteroid -> Asteroid
    update a = a {entityAsteroid = update (entityAsteroid a)}

instance Update Saucer where
    update :: Saucer -> Saucer
    update s = s {entitySaucer = update (entitySaucer s)}

instance Update Bullet where
    update :: Bullet -> Bullet
    update b = b {entityBullet = update e, distance = distance b + speed e}
        where
            e = entityBullet b