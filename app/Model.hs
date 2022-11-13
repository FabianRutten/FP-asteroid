-- | This module contains the data types
--   which represent the state of the game
module Model where

import Animation
    ( Animation,
      playerSpawnAnimation,
      playerThrustAnimation,
      playerDeathAnimation )
import System.Random ( StdGen )
import Graphics.Gloss ( Point, Vector, Picture )
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA ( (+), (-))

addPoint :: Point -> Point -> Point
addPoint a b = a PA.+ b

minPoint :: Point -> Point -> Point
minPoint a b = a PA.- b

--application wide constants
frameRate :: Int
frameRate = 30

--screen
screensize :: Int
screensize = 800

halfscreen :: Float
halfscreen = fromIntegral screensize / 2

blackMargin :: Float
blackMargin = 50

maxBulletDistance :: Float
maxBulletDistance = halfscreen * 1.6

--player
playerDrag :: Float
playerDrag = 0.07 -- 7%

playerThrust :: Float
playerThrust = 2

playerRotateSpeed :: Float
playerRotateSpeed = 0.15

playerMaxSpeed :: Float
playerMaxSpeed = 20


--bullet
bulletSpeed :: Float
bulletSpeed = playerMaxSpeed * 1.5 -- always higher then player!

bulletSize :: Float
bulletSize = 0.5

--asteroid
    --amount of asteroids to spawn based on the score of the player
numberInWave :: Int -> Int
numberInWave score | score >= 9000 = 10
                   | score >= 3000 = 5
                   | score >= 2200 = 4
                   | score >= 550  = 3
                   | otherwise     = 2

    --sizes
sizeBig :: Float
sizeBig = 1.5
sizeMedium :: Float
sizeMedium = 1
sizeSmall :: Float
sizeSmall = 0.5
    --speed ranges
speedBig :: (Float, Float)
speedBig = (0.5, 5)
speedMedium :: (Float, Float)
speedMedium = (1.5, 10)
speedSmall :: (Float, Float)
speedSmall = (2.5, 20)

--saucer speed ranges
speedSaucer :: (Float, Float)
speedSaucer = speedMedium

--saucer size
sizeSaucer :: Float
sizeSaucer = sizeMedium

asteroidSpeed :: Float -> (Float, Float)
asteroidSpeed x | x == sizeBig    = speedBig
                | x == sizeMedium = speedMedium
                | otherwise       = speedSmall


--bitmap sizes in pixels, window is 800 by 800 pixels by default for reference
playerBitmapSize :: Float
playerBitmapSize = 80

asteroidBitmapSize :: Float
asteroidBitmapSize = 90

bulletBitmapSize :: Float
bulletBitmapSize = 10

saucerBitmapSize :: Float
saucerBitmapSize = 180


-- hitboxes for every entity are circles with radius relative to the bitmap size and the size of the entity in game, 
playerRadius :: Float
playerRadius = (playerBitmapSize - (playerBitmapSize / 5)) / 2

asteroidRadius :: Float -> Float
asteroidRadius size = asteroidBitmapSize / 2 * size

bulletRadius :: Float
bulletRadius = bulletBitmapSize / 2

saucerRadius :: Float
saucerRadius = saucerBitmapSize / 2


--scores
asteroidScore :: Asteroid -> Int
asteroidScore a = round $ 50 * size (entityAsteroid a)

saucersScore :: Saucer -> Int
saucersScore s = 200

--initials
initialSpace :: StdGen -> Space
initialSpace = MkSpace initialPlayer [] [] [] Unpaused Alive Unsaved (replicate 3 False) 1 

initialPlayer :: Player
initialPlayer = MkPlayer 
                    (MkEntity 0.5 (0,0) (0,1) 0 playerRadius) 
                    (0,1)
                    3 
                    0 
                    False 
                    playerDeathAnimation 
                    playerSpawnAnimation
                    playerThrustAnimation

data Space = MkSpace { player        :: Player
                     , asteroids     :: [Asteroid]
                     , saucers       :: [Saucer]
                     , bullets       :: [Bullet]
                     , paused        :: Paused
                     , gameState     :: GameState
                     , saved         :: Saved
                     , arrowkeysDown :: [Bool]
                     , time          :: Float
                     , randomSeed    :: StdGen                     
                     }

data Paused = Paused | Unpaused
    deriving (Eq, Show)

data GameState = Alive | GameOver
    deriving (Eq, Show)

data Saved = Saved | Unsaved
    deriving (Eq, Show)

data Entity = MkEntity { size      :: Float
                       , position  :: Point
                       , direction :: Vector
                       , speed     :: Float --pixels p/tick (30/s)
                       , radius    :: Float
                       }


data Saucer   = MkSaucer { entitySaucer   :: Entity
                         , lastManeuver   :: Float
                         , lastShot       :: Float}
                         
newtype Asteroid = MkAst    { entityAsteroid :: Entity}

data Bullet = MkBullet { entityBullet :: Entity
                       , fromPlayer   :: Bool
                       , distance     :: Float
                       }

data Player = MkPlayer { entityPlayer :: Entity
                       , orientation  :: Vector
                       , lives        :: Int
                       , score        :: Int
                       , invincible   :: Bool
                       , death        :: Animation
                       , spawn        :: Animation
                       , thrust       :: Animation
                       }

distance2P :: Point -> Point -> Float
distance2P (x1 , y1) (x2 , y2) = sqrt (x'*x' Prelude.+ y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2