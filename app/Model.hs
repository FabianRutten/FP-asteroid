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
playerDrag = 0

--initials
initialSpace :: Space
initialSpace = MkSpace initialPlayer [] [] [] Unpaused Alive (replicate 3 False)

initialPlayer :: Player
initialPlayer = MkPlayer (MkEntity 1 (-200,0) (0,1) 100) (0,1) 3 0

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

data Entity = MkEntity { size      :: Int
                       , position  :: Point
                       , direction :: Vector 
                       , speed     :: Float --pixels p/tick (30/s)
                       }

newtype Saucer = MkSaucer { entitySaucer :: Entity}
newtype Asteroid = MkAst { entityAsteroid :: Entity}

data Bullet = MkBullet { projectile :: Entity
                       , fromPlayer :: Bool
                       , distance   :: Float
                       }



data Player = MkPlayer { ship        :: Entity
                       , orientation :: Vector
                       , lives       :: Int
                       , score       :: Int
                       }

radians :: Float -> Float --convert degrees to radians
radians x = x / 180 * pi
degrees :: Float -> Float --convert radians to degrees
degrees x = x / pi * 180
