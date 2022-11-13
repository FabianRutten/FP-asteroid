{-# LANGUAGE InstanceSigs #-}
module Tick where
import Model
import Animation
import Collision ( checkCollisions )
import Graphics.Gloss.Data.Point ( Point )
import Graphics.Gloss.Data.Vector ( mulSV )
import Data.Maybe
import Data.List
import System.Random ( StdGen )
import Random
import Saucer

updateTick :: Space -> Space
updateTick =  update . saucerAi . spawnSaucers . spawnAsteroids . checkCollisions . checkAnimations--updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

checkAnimations :: Space -> Space
checkAnimations s = updatePlayerAnimations s
    where
        t = time s
        animationDone anim = running anim && t > startTime anim + duration anim
        updatePlayerAnimations :: Space -> Space
        updatePlayerAnimations s | animationDone d = s{player = rp{invincible = True, spawn = activateAnimation t sp}}
                                 | animationDone sp = s{player = p{invincible = False, spawn = sp{running = False}}}
                                 | not (arrowkeysDown s !! 1)  || animationDone th = s{player = p{thrust = th{running = False}}}
                                 | otherwise = s
            where
                p = player s
                d = death p
                sp = spawn p
                th = thrust p
                rp = resetPlayer (score p) (lives p)

resetPlayer :: Int -> Int -> Player
resetPlayer s l = initialPlayer{score = s, lives = l}

saucerAi :: Space -> Space
saucerAi s = s{saucers = map newSaucer srcs, bullets = newBullets }
    where
        srcs = saucers s
        newSaucer :: Saucer -> Saucer
        newSaucer src | lastManeuver src > secondsBetweenMan = pickNewDirection src s
                      | otherwise = src
        newBullets :: [Bullet]
        newBullets = foldr f (bullets s) srcs
            where
                f sauce blts | lastShot sauce > secondsBetweenShot = saucerCalcBullet (player s) sauce : blts
                             | otherwise = blts


spawnAsteroids :: Space -> Space
spawnAsteroids s@MkSpace{asteroids = (x:xs)} = s
spawnAsteroids s@MkSpace{asteroids = []} = s { asteroids = ast, randomSeed = newSeed }--replicate numberInWave spawnAsteroid
    where
        (ast, newSeed) = newAsteroids (numberInWave ((score . player) s)) ([], randomSeed s)
        newAsteroids :: Int -> ([Asteroid], StdGen) -> ([Asteroid], StdGen)
        newAsteroids 0 tup       = tup
        newAsteroids i (as, gen) = newAsteroids (i-1) (asteroid : as, newGen)
            where
                (asteroid, newGen) = randomBigAsteroid gen


spawnSaucers :: Space -> Space
spawnSaucers s@MkSpace{saucers = []} = s{saucers = [saucer], randomSeed = newSeed}
    where
        (saucer, newSeed) = randomSaucer (randomSeed s)
spawnSaucers s@MkSpace{saucers = xs} = s{saucers = map (\x-> x{lastManeuver = addSecs $ lastManeuver x, lastShot = addSecs $ lastShot x}) xs}
    where
        addSecs :: Float -> Float
        addSecs a = a + (1 / fromIntegral frameRate )







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
                 , time      = time s + 1 / fromIntegral frameRate
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
    update s = s{entitySaucer = update $ entitySaucer s}


instance Update Bullet where
    update :: Bullet -> Bullet
    update b = b {entityBullet = update e, distance = distance b + speed e}
        where
            e = entityBullet b