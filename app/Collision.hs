module Collision where

import Model
import Random
import Data.Maybe
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector


checkCollisions :: Space -> Space
checkCollisions = bulletsWithAsteroids . asteroidsCollisionsWithPlayer

asteroidsCollisionsWithPlayer :: Space -> Space
asteroidsCollisionsWithPlayer s | isNothing hit = s
                                | otherwise = checkLives s {player = resetPlayer (score p + asteroidScore (fromJust hit)) (lives p), asteroids = left}
                             where
                                p = player s
                                (left,hit) = asteroidEntityHit (entityPlayer $ player s) (asteroids s)
checkLives :: Space -> Space
checkLives s | (lives . player) s == 0 = gameOver s
             | otherwise = s



bulletsWithAsteroids :: Space -> Space
bulletsWithAsteroids s = let (bs,as,newScore) = asteroidsBulletHits (bullets s) (asteroids s) (score $ player s)
                         in
                        s {asteroids = as, bullets = bs, player = (player s){score = newScore} }
                   where
                    asteroidsBulletHits :: [Bullet] -> [Asteroid] -> Int -> ([Bullet],[Asteroid],Int) --2 lists of asteroids and bullets that didnt hit anything, so to be displayed. Int is the new score
                    asteroidsBulletHits bs as oldScore = asteroidsBulletHits' bs as ([],[], oldScore)
                            where
                        asteroidsBulletHits' :: [Bullet]
                              -> [Asteroid]
                              -> ([Bullet],[Asteroid],Int)
                              -> ([Bullet],[Asteroid],Int)
                        asteroidsBulletHits' [] as (b1,_,newScore) = (b1, as,newScore)
                        asteroidsBulletHits' bs [] (b1,_,newScore) = (bs++b1,[],newScore)
                        asteroidsBulletHits' (b:bs) ast (b1,_,newScore)
                               | isNothing hit = asteroidsBulletHits' bs ast  (b:b1,[],newScore)
                               | otherwise     = asteroidsBulletHits' bs left  (b1, [], newScore + getScore b (fromJust hit))
                            where
                              (left,hit) = asteroidEntityHit (entityBullet b) ast
                              getScore :: Bullet -> Asteroid -> Int
                              getScore b a | fromPlayer b = asteroidScore a
                                           | otherwise = 0

asteroidEntityHit :: Entity -> [Asteroid] -> ([Asteroid], Maybe Asteroid) --left list is to stay, right is a maybe asteroid that is hit
asteroidEntityHit e as = asteroidEntityHit' e as ([],Nothing)
              where
        asteroidEntityHit' _ [] hit = hit
        asteroidEntityHit' e (a:as) (left,_) | checkHit e (entityAsteroid a) = (left++as, Just a)
                                             | otherwise = asteroidEntityHit' e as (a:left, Nothing)

spawnChildAsteroids :: Asteroid -> [Asteroid]
spawnChildAsteroids a | sizeA > sizeSmall = replicate 2 (spawnChildAsteroid (sizeA - 0.5))
                      | otherwise = []
                    where
                     sizeA = size (entityAsteroid a)
                     spawnChildAsteroid sizeNew = MkAst $ MkEntity sizeNew (position (entityAsteroid a)) randomDirection randomSpeed (asteroidRadius sizeNew)



checkHit :: Entity -> Entity -> Bool
checkHit a b = distance2P (position a) (position b) <= (radius a + radius b)

distance2P :: Point -> Point -> Float
distance2P (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2


resetPlayer :: Int -> Int -> Player
resetPlayer s l = initialPlayer{lives = l -1, score = s}

gameOver :: Space -> Space
gameOver s = s{gameState = GameOver}