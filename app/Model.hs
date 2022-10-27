-- | This module contains the data types
--   which represent the state of the game
module Model where

import Entity
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point.Arithmetic

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
initialSpace = MkSpace initialPlayer [] [] [] False (replicate 3 False)

initialPlayer :: Player
initialPlayer = MkPlayer (MkEntity 1 (0,0) (0,0) 100) (0,0) 3 0

data Space = MkSpace { player        :: Player
                     , asteroids     :: [Asteroid]
                     , saucers       :: [Saucer]
                     , bullets       :: [Bullet]
                     , paused        :: Bool
                     , arrowkeysDown :: [Bool]
                     }

type Saucer = Entity
type Asteroid = Entity

data Bullet = MkBullet { projectile :: Entity
                       , fromPlayer :: Bool
                       , distance   :: Float
                       }



data Player = MkPlayer { ship        :: Entity
                       , orientation :: Vector
                       , lives       :: Int
                       , score       :: Int
                       }

