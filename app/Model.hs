-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss ( Point, Vector, Picture )
import Graphics.Gloss.Data.Point.Arithmetic ( (+) )

addPoint :: Point -> Point -> Point
addPoint a b = a Graphics.Gloss.Data.Point.Arithmetic.+ b

--application wide constants
screensize :: Int
screensize = 800

blackMargin :: Int
blackMargin = 100

floatBlackMargin :: Float
floatBlackMargin = fromIntegral blackMargin

halfscreen :: Float
halfscreen = fromIntegral screensize / 2

playerDrag :: Float
playerDrag = 7 -- in percentile 0-100. 100 being higher drag
playerThrust :: Float
playerThrust = 2
playerMaxSpeed :: Float
playerMaxSpeed = 20

bulletSpeed :: Float
bulletSpeed = 30 -- always higher then player!
bulletRadius :: Float
bulletRadius = 30

playerRotateSpeed :: Float
playerRotateSpeed = 0.09

--asteroid
nextWaveScores :: [Int]
nextWaveScores = [100,200]
    --Sizes
sizeBig :: Float
sizeBig = 1.5
sizeMedium :: Float
sizeMedium = 1
sizeSmall :: Float
sizeSmall = 0.5
    --speeds
speedBig :: Float
speedBig = 1
speedMedium :: Float
speedMedium = 1.5
speedSmall :: Float
speedSmall = 2


numberInWave :: Int
numberInWave = 1

    --hitboxes
playerBitmapSize :: Float -- size of bitmap in pixels
playerBitmapSize = 80

playerRadius :: Float
playerRadius = (playerBitmapSize - (playerBitmapSize / 5)) / 2

    --hitboxes
asteroidBitmapSize :: Float -- size of bitmap in pixels
asteroidBitmapSize = 90

asteroidRadius :: Float -> Float
asteroidRadius size = asteroidBitmapSize / 2 * size
    --scores
asteroidScore :: Asteroid -> Int
asteroidScore a = round $ 20.0 * size (entityAsteroid a)

-- asteroidScore a | getSize == sizeBig = 100
--                 | getSize == sizeMedium = 70
--                 | getSize == sizeSmall = 30
--                 | otherwise = 0  --pattern matchin (could in theory not be hit, but just to be sure
--                 where
--                     getSize = size . entityAsteroid $ a

--initials
initialSpace :: Space
initialSpace = MkSpace initialPlayer [] [] [] Unpaused Alive (replicate 3 False)

initialPlayer :: Player
initialPlayer = MkPlayer (MkEntity 0.5 (0,0) (0,1) 2 playerRadius) (0,1) 3 0

data Space = MkSpace { player        :: Player
                     , asteroids     :: [Asteroid]
                     , saucers       :: [Saucer]
                     , bullets       :: [Bullet]
                     , paused        :: Paused
                     , gameState     :: GameState
                     , arrowkeysDown :: [Bool]
                     }

data Paused = Paused | Unpaused
    deriving (Eq, Show)

data GameState = Alive | GameOver
    deriving (Eq, Show)

data Entity = MkEntity { size      :: Float
                       , position  :: Point
                       , direction :: Vector 
                       , speed     :: Float --pixels p/tick (30/s)
                       , radius    :: Float
                       }

newtype Saucer = MkSaucer { entitySaucer :: Entity}
newtype Asteroid = MkAst { entityAsteroid :: Entity}

data Bullet = MkBullet { entityBullet :: Entity
                       , fromPlayer :: Bool
                       , distance   :: Float
                       }

data Player = MkPlayer { entityPlayer        :: Entity
                       , orientation :: Vector
                       , lives       :: Int
                       , score       :: Int
                       }

radians :: Float -> Float --convert degrees to radians
radians x = x / 180 * pi
degrees :: Float -> Float --convert radians to degrees
degrees x = x / pi * 180

