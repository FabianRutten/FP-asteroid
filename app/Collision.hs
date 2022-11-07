module Collision where

import Model
import Random
import Data.Maybe
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import System.Random (StdGen)


checkCollisions :: Space -> Space
checkCollisions = bulletsWithAsteroids . asteroidsCollisionsWithPlayer

asteroidsCollisionsWithPlayer :: Space -> Space
asteroidsCollisionsWithPlayer s | isNothing hit = s
                                | otherwise = checkLives s {player = resetPlayer (score p + asteroidScore (fromJust hit)) (lives p), asteroids = left, randomSeed = seed}
                             where
                                p = player s
                                (left,hit,seed) = asteroidEntityHit (randomSeed s) (entityPlayer $ player s) (asteroids s)
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
                                  (left,hit,seed) = asteroidEntityHit (randomSeed s) (entityBullet b) ast
                                  getScore :: Bullet -> Asteroid -> Int
                                  getScore b a | fromPlayer b = asteroidScore a
                                               | otherwise = 0

asteroidEntityHit :: StdGen -> Entity -> [Asteroid] -> ([Asteroid], Maybe Asteroid,StdGen) --left list is to stay, right is a maybe asteroid that is hit
asteroidEntityHit gen e as = asteroidEntityHit' e as ([],Nothing,gen)
              where
        asteroidEntityHit' :: Entity -> [Asteroid] -> ([Asteroid], Maybe Asteroid,StdGen) -> ([Asteroid], Maybe Asteroid,StdGen)
        asteroidEntityHit' _ [] hit = hit
        asteroidEntityHit' e (a:as) (left,_,g0)| checkHit e (entityAsteroid a) = (left++as++childAsts, Just a, seed)
                                                | otherwise = asteroidEntityHit' e as (a:left, Nothing, seed)
                                    where
                                        (childAsts, seed) = spawnChildAsteroids g0 a

spawnChildAsteroids :: StdGen -> Asteroid -> ([Asteroid],StdGen)
spawnChildAsteroids gen a | sizeA > sizeSmall = ([a1,a2],g2)
                          | otherwise = ([],gen)
                    where
                     (a1,g1) = spawnChildAsteroid gen (sizeA - 0.5)
                     (a2,g2) = spawnChildAsteroid g1 (sizeA - 0.5)
                     sizeA = size (entityAsteroid a)
                     spawnChildAsteroid :: StdGen -> Float -> (Asteroid,StdGen)
                     spawnChildAsteroid g sizeNew = (MkAst $ MkEntity sizeNew (position (entityAsteroid a)) rdmD rdmS (asteroidRadius sizeNew) [],g2)
                        where
                      (rdmD, g1) = randomDirection g
                      (rdmS, g2) = randomSpeed g1



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