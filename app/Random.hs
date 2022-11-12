module Random where

import Model
import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random.Stateful (STGen)
import Data.Fixed (mod')

randomLocation :: StdGen -> (Point, StdGen)
randomLocation gen = (point, newgen)
    where 
        (location, newgen) = randomR (0, 1600) gen
        point | location > 1200 = (-400, location - 1200)
              | location > 800  = (location - 800, -400)
              | location > 400  = (400, location - 400)
              | otherwise       = (location, 400)


randomDirection :: StdGen -> (Vector, StdGen)
randomDirection gen = ((outputx, ouputy),newSeed)
    where
        (outputx, seed) = randomR (-1.0,1.0) gen
        (ouputy, newSeed) = randomR (-1.0,1.0) seed


randomSpeed :: StdGen -> (Float, Float) -> (Float, StdGen)
randomSpeed gen range = output
    where 
        (output, seed) = (randomR range gen , seed)

randomBigAsteroid :: StdGen -> (Asteroid, StdGen)
randomBigAsteroid gen = (MkAst $ MkEntity sizeBig rndPoint rndDirection rndSpeed (asteroidRadius sizeBig ), gen3)
    where
        (rndPoint,     gen1) = randomLocation  gen
        (rndDirection, gen2) = randomDirection gen1
        (rndSpeed,     gen3) = randomSpeed     gen2 (asteroidSpeed sizeBig)