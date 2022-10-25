-- | This module contains the data types
--   which represent the state of the game
module Model where

import Entity
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

--initials
initialSpace :: Space
initialSpace = MkSpace initialPlayer [] [] [] True (replicate 3 False)

initialPlayer :: Player
initialPlayer = MkPlayer (MkEntity 1 (400,400) (0,0) 0) (0,0) 3 0

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

