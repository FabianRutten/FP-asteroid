-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss ( Point, Vector, Picture )
import Graphics.Gloss.Data.Point.Arithmetic ( (+) )
import System.Random.Stateful (STGen)

addPoint :: Point -> Point -> Point
addPoint a b = a Graphics.Gloss.Data.Point.Arithmetic.+ b

--application wide constants

--screen
screensize :: Int
screensize = 800

halfscreen :: Float
halfscreen = fromIntegral screensize / 2

blackMargin :: Float
blackMargin = 50


--player
playerDrag :: Float
playerDrag = 0.07 -- 7%

playerThrust :: Float
playerThrust = 2

playerRotateSpeed :: Float
playerRotateSpeed = 0.09

playerMaxSpeed :: Float
playerMaxSpeed = 20


--bullet
bulletSpeed :: Float
bulletSpeed = playerMaxSpeed * 1.5 -- always higher then player!


--asteroid
numberInWave :: Int
numberInWave = 1

    --sizes
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


--bitmap sizes in pixels, window is 800 by 800 pixels by default for reference
playerBitmapSize :: Float
playerBitmapSize = 80

asteroidBitmapSize :: Float
asteroidBitmapSize = 90

bulletBitmapSize :: Float
bulletBitmapSize = 10


-- hitboxes for every entity are circles with radius relative to the bitmap size and the size of the entity in game, 
playerRadius :: Float
playerRadius = (playerBitmapSize - (playerBitmapSize / 5)) / 2

asteroidRadius :: Float -> Float
asteroidRadius size = asteroidBitmapSize / 2 * size

bulletRadius :: Float
bulletRadius = bulletBitmapSize / 2


--scores
asteroidScore :: Asteroid -> Int
asteroidScore a = round $ 50 * size (entityAsteroid a)

saucersScore :: Saucer -> Int
saucersScore s = 200


--initials
initialSpace :: StdGen -> Space
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
                     , randomSeed    :: StdGen
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

newtype Saucer   = MkSaucer { entitySaucer   :: Entity}
newtype Asteroid = MkAst    { entityAsteroid :: Entity}

data Bullet = MkBullet { entityBullet :: Entity
                       , fromPlayer   :: Bool
                       , distance     :: Float
                       }

data Player = MkPlayer { entityPlayer :: Entity
                       , orientation  :: Vector
                       , lives        :: Int
                       , score        :: Int
                       }