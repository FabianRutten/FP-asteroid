{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Tick where

import Model
import Data.Data (Data)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import System.Random



updateTick :: Space -> Space
updateTick = updatePlayer . updateAsteroids . updateBullets . updateSaucers . checkCollisions

updateAsteroids :: Space -> Space
updateAsteroids s | score (player s) == 0 && null (asteroids s) = s {asteroids = spawnNew}
                  | score (player s) `elem` nextWaveScores = s {asteroids = spawnNew}
                  | otherwise = s {asteroids = map updateAsteroid $ asteroids s}
                where
                    spawnNew = replicate numberInWave spawnAsteroid

spawnAsteroid :: Asteroid
spawnAsteroid = MkAst $ MkEntity sizeBig pickPoint pickDirection speedBig $ asteroidRadius sizeBig
           where
            xOry = True --for now, needs to be random
            pickPoint | xOry = (0,0)
                      | otherwise = (400,400)
            pickDirection = (1,4)

updateBullets :: Space -> Space
updateBullets s = s {bullets = map updateBullet $ checkBulletsDistance $ bullets s}
                where
                    checkBulletsDistance :: [Bullet] -> [Bullet]
                    checkBulletsDistance = filter (\x -> distance x < halfscreen)


updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {ship = (updateShipPosition . dragShip) (ship p)}
                 in s {player = update $ player s}
            where
                updateShipPosition :: Entity -> Entity
                updateShipPosition = updateEntityPosition
                dragShip :: Entity -> Entity
                dragShip s = s {speed = max 0 (speed s - speed s * playerDrag / 100)}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a = MkAst $ updateEntityPosition $ entityAsteroid a

updateBullet :: Bullet -> Bullet
updateBullet = updateBulletPosition . updateBulletDistance
            where
                updateBulletPosition b = b {projectile = updateEntityPosition (projectile b)}
                updateBulletDistance b = b {distance = distance b + speed (projectile b)}



updateSaucer :: Saucer -> Saucer
updateSaucer s = s


checkPoint :: Point -> Point --screensize/2 + floatBlackMargin (dark place) as maximum point values. with negatives as well -> swap sides
checkPoint old@(x,y) | x >  (halfscreen + floatBlackMargin)    = ((-halfscreen) -floatBlackMargin, y)
                     | y >  (halfscreen + floatBlackMargin)    = (x, (-halfscreen)-floatBlackMargin)
                     | x <  ((-halfscreen) - floatBlackMargin) = (halfscreen+floatBlackMargin, y)
                     | y <  ((-halfscreen) - floatBlackMargin) = (x, halfscreen+floatBlackMargin)
                     | otherwise = old

--first update position, simply with direction and speed. 
--Then check if new position is outside of set bounderies of the play space and 
updateEntityPosition :: Entity -> Entity
updateEntityPosition a = a{ position = checkPoint (movePoint a)}
                where
                    movePoint :: Entity -> Point
                    movePoint a = mulSV (speed a) (direction a) `addPoint` position a


checkCollisions :: Space -> Space
checkCollisions = bulletsWithAsteroids . asteroidsCollisionsWithPlayer

asteroidsCollisionsWithPlayer :: Space -> Space
asteroidsCollisionsWithPlayer s | isHit = if lives (player s) > 1
                                          then s {player = (player s){score = score (player s) - 1}}
                                          else gameOver s
                                | otherwise = s
                    where
                        isHit             :: Bool
                        isHit                 = any (\a -> asteroidHitPlayer a $ player s) $ asteroids s
                        asteroidHitPlayer :: Asteroid -> Player -> Bool
                        asteroidHitPlayer a p = checkHit (entityAsteroid a) $ ship p


bulletsWithAsteroids :: Space -> Space
bulletsWithAsteroids s = let ((as,bs),matches) = asteroidHits (bullets s) (asteroids s)
                         in
                         setAsteroids as $ setBullets bs $ setScore (getScore matches) s
                   where
                    setAsteroids :: [Asteroid] -> Space -> Space
                    setAsteroids a s = s {asteroids = a}
                    setBullets   ::  [Bullet] -> Space -> Space
                    setBullets b s   = s {bullets = b}
                    setScore :: Int -> Space -> Space
                    setScore score s = s {player = (player s){score = score}}
                    getScore :: [(Bullet, Asteroid)] -> Int
                    getScore [] = 0
                    getScore (x@(b,a):xs) | fromPlayer b = asteroidScore (size $ entityAsteroid a) + getScore xs
                                          | otherwise = getScore xs

                    asteroidHits :: [Bullet] -> [Asteroid] -> (([Asteroid],[Bullet]),[(Bullet,Asteroid)])
--(([asteroids that will be displayed, so not destroyed],[Bullets that will be displayed, so havent hit anything yet]),[all hit matches, there is ALWAYS one bullet and one possible asteroid])
                    asteroidHits bs as = recur bs as (([],[]),[])
                            where
                        recur :: [Bullet] -> [Asteroid]
                                  -> (([Asteroid],[Bullet]),[(Bullet,Asteroid)])
                                  -> (([Asteroid],[Bullet]),[(Bullet,Asteroid)])
                        recur [] as ((a1,b1),match) = ((as++a1,b1),match)
                        recur bs [] ((a1,b1),match) = ((a1,bs++b1),match)
                        recur (b:bs) (a:as) ((a1,b1),match)
                               | checkHit (projectile b) (entityAsteroid a) = recur bs as ((a1,b1), (b,a):match)
                               | otherwise = recur bs as ((a:a1,b:b1),match)






--not sure if needed anymore
headm :: [a] -> Maybe a --https://stackoverflow.com/questions/54015516/get-first-element-of-list-as-maybe-vs-maybe-elements
headm []     = Nothing
headm (x:xs) = Just x

--not sure if needed anymore
createCombos :: [a] -> [b] -> [(a,b)]
createCombos a b = (,) <$> a <*> b


checkHit :: Entity -> Entity -> Bool
checkHit x y = undefined

gameOver :: Space -> Space
gameOver s = s{gameState = GameOver}
