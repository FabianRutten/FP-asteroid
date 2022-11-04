{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
module Tick where

import Model
import Collision
import Data.Data
import Data.Maybe
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import System.Random


updateTick :: Space -> Space
updateTick =  update . checkCollisions --updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

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


checkPoint :: Point -> Point --screensize/2 + blackMargin (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint old@(x,y) | x >  (halfscreen + blackMargin)    = ((-halfscreen) -blackMargin, y)
                     | y >  (halfscreen + blackMargin)    = (x, (-halfscreen)-blackMargin)
                     | x <  ((-halfscreen) - blackMargin) = (halfscreen+blackMargin, y)
                     | y <  ((-halfscreen) - blackMargin) = (x, halfscreen+blackMargin)
                     | otherwise = old


class Update a where
    update :: a -> a

instance Update a => Update [a] where 
    update :: Update a => [a] -> [a]
    update = map update

instance Update Space where 
    update :: Space -> Space
    update s = s { player    = update (player s), 
                   asteroids = updateAsteroids (asteroids s), 
                   saucers   = update (saucers s),
                   bullets   = update (filter (\x -> distance x < halfscreen) (bullets s))
                 }

instance Update Entity where
    --first update position, simply with direction and speed. 
    --then check if new position is outside of set bounderies of the play space and 
    update :: Entity -> Entity
    update e = e { position = checkPoint $ mulSV (speed e) (direction e) `addPoint` position e}

instance Update Player where
    update :: Player -> Player
    update p = p {entityPlayer = (update . drag) (entityPlayer p)}
        where
            drag p = p {speed = max 0 (speed p - speed p * playerDrag)}

instance Update Asteroid where
    update :: Asteroid -> Asteroid
    update a = a { entityAsteroid = update (entityAsteroid a) }

updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids as | null as   = spawnAsteroid --replicate numberInWave spawnAsteroid
                   | otherwise = update as

instance Update Bullet where
    update :: Bullet -> Bullet
    update b = b {entityBullet = update e, distance = distance b + speed e}
        where
            e = entityBullet b

instance Update Saucer where
    update :: Saucer -> Saucer
    update s = s { entitySaucer = update (entitySaucer s) }


            




