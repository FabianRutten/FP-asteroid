module Collision (checkCollisions) where

import Model
import Random ( randomDirection, randomSpeed )
import Data.Maybe ( fromJust, isNothing )
import System.Random (StdGen)
import Animation ( Animation(running, startTime) )


checkCollisions :: Space -> Space
checkCollisions = bulletsWithAsteroids . asteroidsCollisionsWithPlayer . playerWithSaucers . bulletsWithSaucer . bulletsWithPlayer . asteroidsWithSaucers

asteroidsCollisionsWithPlayer :: Space -> Space
asteroidsCollisionsWithPlayer s | invincible p = s
                                | isNothing hit = s
                                | otherwise = checkLives s {player = playerDeath (time s) p{score = score p + asteroidScore (fromJust hit)}, asteroids = left, randomSeed = seed}
                             where
                                p = player s
                                (left,hit,seed) = asteroidEntityHit (randomSeed s) (entityPlayer $ player s) (asteroids s)
checkLives :: Space -> Space
checkLives s | (lives . player) s == 0 = gameOver s
             | otherwise = s

playerDeath :: Float -> Player -> Player
playerDeath secs p = p{death = d{running = True, startTime = secs}
                      , lives = max 0 (lives p - 1)
                      , invincible = True}
                 where
                  d = death p


bulletsWithAsteroids :: Space -> Space
bulletsWithAsteroids s = let (bs,as,newScore, newSeed) = asteroidsBulletHits (bullets s) (asteroids s) (score $ player s)
                         in
                        s {randomSeed = newSeed, asteroids = as, bullets = bs, player = (player s){score = newScore} }
                   where
                    asteroidsBulletHits :: [Bullet] -> [Asteroid] -> Int -> ([Bullet],[Asteroid],Int, StdGen) --2 lists of asteroids and bullets that didnt hit anything, so to be displayed. Int is the new score
                    asteroidsBulletHits bs as oldScore = asteroidsBulletHits' bs as ([],[], oldScore, randomSeed s)
                            where
                        asteroidsBulletHits' :: [Bullet]
                              -> [Asteroid]
                              -> ([Bullet],[Asteroid],Int,StdGen)
                              -> ([Bullet],[Asteroid],Int,StdGen)
                        asteroidsBulletHits' [] as (b1,_,newScore, gen) = (b1, as,newScore, gen)
                        asteroidsBulletHits' bs [] (b1,_,newScore, gen) = (bs++b1,[],newScore, gen)
                        asteroidsBulletHits' (b:bs) ast (b1,_,newScore, gen)
                                      | isNothing hit = asteroidsBulletHits' bs ast  (b:b1,[],newScore, gen)
                                      | otherwise     = asteroidsBulletHits' bs left  (b1, [], newScore + getScore b (fromJust hit), seed)
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
        asteroidEntityHit' e (a:as) (left,_,g0) | checkHit e (entityAsteroid a) = (left++as++childAsts, Just a, seed)
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
                     spawnChildAsteroid g sizeNew = (MkAst $ MkEntity sizeNew (position (entityAsteroid a)) rdmD rdmS (asteroidRadius sizeNew),g2)
                        where
                      (rdmD, g1) = randomDirection g
                      (rdmS, g2) = randomSpeed g1 (asteroidSpeed sizeNew)



checkHit :: Entity -> Entity -> Bool
checkHit a b = distance2P (position a) (position b) <= (radius a + radius b)

saucerEntityHit :: [Saucer] -> Entity -> ([Saucer],Maybe Saucer)
saucerEntityHit scrs e = foldr f ([],Nothing) scrs
      where
            f :: Saucer -> ([Saucer],Maybe Saucer) -> ([Saucer],Maybe Saucer)
            f x (left,hit) | checkHit (entitySaucer x) e = (left,Just x)
                           | otherwise                   = (x:left, hit)

playerWithSaucers :: Space -> Space
playerWithSaucers s | invincible p  = s
                    | isNothing hit = s
                    | otherwise     = checkLives s {player = playerDeath (time s) p{score = score p + saucersScore (fromJust hit)}, saucers = left}
                             where
                                p          = player s
                                (left,hit) = saucerEntityHit (saucers s) (entityPlayer p)

bulletsWithPlayer :: Space -> Space
bulletsWithPlayer s | invincible p = s
                    | isNothing hit = s
                    | otherwise = checkLives s{player = playerDeath (time s) p}
      where
            p = player s
            nonPBullets = filter (not . fromPlayer) (bullets s)
            hit :: Maybe Bullet
            hit = foldr f Nothing nonPBullets
                  where
                        f b mB | checkHit (entityBullet b) (entityPlayer p) = Just b
                               | otherwise                                  = mB

bulletsWithSaucer :: Space -> Space
bulletsWithSaucer s = let (bs,scrs,newScore) = saucersBulletHits (bullets s) (saucers s) (score $ player s)
                         in
                        s {saucers = scrs, bullets = bs, player = (player s){score = newScore} }
                   where
                    saucersBulletHits :: [Bullet] -> [Saucer] -> Int -> ([Bullet],[Saucer],Int) --2 lists of asteroids and bullets that didnt hit anything, so to be displayed. Int is the new score
                    saucersBulletHits bs as oldScore = saucersBulletHits' bs as ([],[], oldScore)
                            where
                        saucersBulletHits' :: [Bullet]
                              -> [Saucer]
                              -> ([Bullet],[Saucer],Int)
                              -> ([Bullet],[Saucer],Int)
                        saucersBulletHits' [] as (b1,_,newScore) = (b1, as,newScore)
                        saucersBulletHits' bs [] (b1,_,newScore) = (bs++b1,[],newScore)
                        saucersBulletHits' (b:bs) ast (b1,_,newScore)
                                      | isNothing hit = saucersBulletHits' bs ast  (b:b1,[],newScore)
                                      | otherwise                           = saucersBulletHits' bs left  (b1, [], newScore + getScore b (fromJust hit))
                                where
                                  (left,hit) = saucerEntityHit ast (entityBullet b)
                                  getScore :: Bullet -> Saucer -> Int
                                  getScore b a | fromPlayer b = saucersScore a
                                               | otherwise = 0

asteroidsWithSaucers :: Space -> Space
asteroidsWithSaucers s = s{asteroids = ast, saucers = left, randomSeed = newSeed}
      where
            (ast,left,newSeed) = foldr f (asteroids s, [], randomSeed s) (saucers s)
                  where
                        f saucer all@(ast,left, gen) | isNothing ahit = (aLeft, saucer : left ,gen0)
                                                     | otherwise = (aLeft,left,gen0)
                              where
                                    (aLeft,ahit,gen0) = asteroidEntityHit gen (entitySaucer saucer) ast

gameOver :: Space -> Space
gameOver s = s{gameState = GameOver}