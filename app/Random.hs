module Random where

import System.Random
import Graphics.Gloss.Data.Vector
import System.Random.Stateful (STGen)

randomDirection :: StdGen -> (Vector, StdGen)
randomDirection gen = ((outputx, ouputy),newSeed)
        where
            (outputx, seed) = randomR (-1.0,1.0) gen
            (ouputy, newSeed) = randomR (-1.0,1.0) seed


randomSpeed :: StdGen -> (Float, StdGen)
randomSpeed gen = output
        where 
            (output, seed) = (randomR (0.5,30.0) gen , seed)