module Collision where

import Model
import Data.Maybe
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector


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
                              (left,hit) = asteroidHit (entityBullet b) ast
                              getScore :: Bullet -> Asteroid -> Int
                              getScore b a | fromPlayer b = asteroidScore a
                                           | otherwise = 0

asteroidHit :: Entity -> [Asteroid] -> ([Asteroid], Maybe Asteroid) --left list is to stay, right is a maybe asteroid that is hit
asteroidHit e as = asteroidHit' e as ([],Nothing)
              where
        asteroidHit' _ [] hit = hit
        asteroidHit' e (a:as) (left,_) | checkHit e (entityAsteroid a) = (left++as, Just a)
                                       | otherwise = asteroidHit' e as (a:left, Nothing)

checkHit :: Entity -> Entity -> Bool
checkHit a b = distance2P (position a) (position b) <= (radius a + radius b)

distance2P :: Point -> Point -> Float
distance2P (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2


resetPlayer :: Int -> Player
resetPlayer x = initialPlayer{lives = x}

gameOver :: Space -> Space
gameOver s = s{gameState = GameOver}