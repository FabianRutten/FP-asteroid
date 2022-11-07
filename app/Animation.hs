module Animation where

import Graphics.Gloss


data Animation = MkAnimation { atype :: AType
                             , running :: Bool
                             , aframes :: [AFrame]}

data AType = Death | Spawn | Thrust
    deriving (Eq, Show)
                             
data AFrame = MkAFrame { picture :: Picture
                       , timing :: Float}  

aTime :: Float -> Animation -> Animation
aTime secs a | q == Death = aDeathTime a
             | q == Spawn = aSpawnTime a
             | otherwise = aThrustTime a
            where
                q = atype a

aDeathTime = undefined
aSpawnTime = undefined
aThrustTime = undefined