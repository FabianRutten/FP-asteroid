{-# LANGUAGE InstanceSigs #-}
module Tick where

import Model
import Animation
import Collision ( checkCollisions )
import Graphics.Gloss.Data.Point ( Point )
import Graphics.Gloss.Data.Vector ( mulSV )
import Data.Maybe
import Data.List

updateTick :: Space -> Space
updateTick =  update . spawnAsteroids . checkCollisions . checkAnimations--updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

checkAnimations :: Space -> Space
checkAnimations s = updateTheRestAnimations $ updatePlayerAnimations s
    where
        t = time s
        animationDone anim = running anim && not (any(\x-> timing x > t) $ aframes anim)
        updatePlayerAnimations :: Space -> Space
        updatePlayerAnimations s | animationDone d = s{player = rp{noKill = True, spawn = sp{running = True, aframes = setAFramesTimes t $ aframes sp}}
                                                          }
                                 | animationDone sp = s{player = p{noKill = False, spawn = sp{running = False}}}
                                 | animationDone th = undefined
                                 | otherwise = s
            where
                p = player s
                d = death p
                sp = spawn p
                th = thrust p
                rp = resetPlayer (score p) (lives p)

        updateTheRestAnimations :: Space -> Space
        updateTheRestAnimations s = s

resetPlayer :: Int -> Int -> Player
resetPlayer s l = initialPlayer{score = s, lives = l}


spawnAsteroids :: Space -> Space
spawnAsteroids s | null (asteroids s) = s { asteroids = spawnAsteroid }--replicate numberInWave spawnAsteroid
                 | otherwise = s
    where
        spawnAsteroid :: [Asteroid]
        spawnAsteroid = [MkAst $ MkEntity sizeBig    pickPoint pickDirectionB speedBig    (asteroidRadius sizeBig ),
                         MkAst $ MkEntity sizeMedium pickPoint pickDirectionM speedMedium (asteroidRadius sizeMedium),
                         MkAst $ MkEntity sizeSmall  pickPoint pickDirectionS speedSmall  (asteroidRadius sizeSmall)]
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
    update s = s { player    = update (player s)
                 , asteroids = update (asteroids s)
                 , saucers   = update (saucers s)
                 , bullets   = update (filter (\x -> distance x < halfscreen) (bullets s))
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
    update p | running (death p) = p
             | otherwise = p {entityPlayer = (update . drag) (entityPlayer p)}
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