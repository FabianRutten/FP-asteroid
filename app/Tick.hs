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
updateAsteroids s | null (asteroids s) = s {asteroids = spawnNew}
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
                    updateBullet :: Bullet -> Bullet
                    updateBullet = updateBulletPosition . updateBulletDistance
                           where
                        updateBulletPosition b = b {entityBullet = updateEntityPosition (entityBullet b)}
                        updateBulletDistance b = b {distance = distance b + speed (entityBullet b)}


updateSaucers :: Space -> Space
updateSaucers s = s {saucers = map updateSaucer $ saucers s}

updatePlayer :: Space -> Space
updatePlayer s = let update p = p {entityPlayer = (updateentityPlayerPosition . dragEntityPlayer) (entityPlayer p)}
                 in s {player = update $ player s}
            where
                updateentityPlayerPosition :: Entity -> Entity
                updateentityPlayerPosition = updateEntityPosition
                dragEntityPlayer :: Entity -> Entity
                dragEntityPlayer s = s {speed = max 0 (speed s - speed s * playerDrag / 100)}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a = a { entityAsteroid = updateEntityPosition $ entityAsteroid a }

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
                                          then s {player = (player s){lives = lives (player s) - 1}}
                                          else gameOver s
                                | otherwise = s
                    where
                        isHit             :: Bool
                        isHit                 = any (\a -> asteroidHitPlayer a $ player s) $ asteroids s
                        asteroidHitPlayer :: Asteroid -> Player -> Bool
                        asteroidHitPlayer a p = checkHit (entityAsteroid a) $ entityPlayer p


bulletsWithAsteroids :: Space -> Space
bulletsWithAsteroids s = let (as,bs,newScore) = asteroidHits (bullets s) (asteroids s) (score $ player s)
                         in
                        s {asteroids = as, bullets = bs, player = (player s){score = newScore} } 
                   where
                    
                    asteroidHits :: [Bullet] -> [Asteroid] -> Int -> ([Asteroid],[Bullet],Int)
--(([asteroids that will be displayed, so not destroyed],[Bullets that will be displayed, so havent hit anything yet]),[all hit newScorees, there is ALWAYS one bullet and one possible asteroid])
                    asteroidHits bs as oldScore = recur bs as ([],[], oldScore)
                            where
                        recur :: [Bullet] 
                              -> [Asteroid]
                              -> ([Asteroid],[Bullet],Int)
                              -> ([Asteroid],[Bullet],Int)
                        recur [] as (a1,b1,newScore) = (as++a1,b1,newScore)
                        recur bs [] (a1,b1,newScore) = (a1,bs++b1,newScore)
                        recur (b:bs) (a:as) (a1,b1,newScore)
                               | checkHit (entityBullet b) (entityAsteroid a) = recur bs as (a1,b1, newScore + getScore b a)
                               | otherwise = recur bs as (a:a1,b:b1,newScore)
                            where
                              getScore :: Bullet -> Asteroid -> Int
                              getScore b a | fromPlayer b = asteroidScore a
                                           | otherwise = 0

--not sure if needed anymore
headm :: [a] -> Maybe a --https://stackoverflow.com/questions/54015516/get-first-element-of-list-as-maybe-vs-maybe-elements
headm []     = Nothing
headm (x:xs) = Just x

--not sure if needed anymore
createCombos :: [a] -> [b] -> [(a,b)]
createCombos a b = (,) <$> a <*> b


checkHit :: Entity -> Entity -> Bool
checkHit a b = distance2P (position a) (position b) <= (radius a + radius b)

distance2P :: Point -> Point -> Float
distance2P (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2

gameOver :: Space -> Space
gameOver s = s{gameState = GameOver}
